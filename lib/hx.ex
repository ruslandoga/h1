defmodule Hx do
  @moduledoc "A minimal HTTP/1 server."
  use GenServer
  require Logger

  require Record
  Record.defrecordp(:timeouts, [:accept, :request, :headers])
  Record.defrecordp(:state, [:socket, :acceptors, :timeouts, :plug])

  def start_link(opts) do
    {genserver_opts, opts} = Keyword.split(opts, [:name, :debug])
    GenServer.start_link(__MODULE__, opts, genserver_opts)
  end

  def acceptors(pid) do
    acceptors = GenServer.call(pid, :acceptors)
    Enum.map(:ets.tab2list(acceptors), &elem(&1, 0))
  end

  def sockname(pid) do
    socket = GenServer.call(pid, :socket)
    :socket.sockname(socket)
  end

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)

    plug = Keyword.fetch!(opts, :plug)
    addr = Keyword.get(opts, :addr, {127, 0, 0, 1})
    port = Keyword.get(opts, :port, 0)
    min_acceptors = Keyword.get(opts, :acceptors, 100)
    timeouts = Keyword.get(opts, :timeouts, [])

    timeouts =
      timeouts(
        accept: Keyword.get(timeouts, :accept, :timer.seconds(10)),
        request: Keyword.get(timeouts, :request, :timer.seconds(60)),
        headers: Keyword.get(timeouts, :headers, :timer.seconds(10))
      )

    {:ok, socket} = :socket.open(:inet, :stream, :tcp)
    :ok = :socket.setopt(socket, {:socket, :reuseaddr}, true)
    :ok = :socket.setopt(socket, {:socket, :linger}, %{onoff: true, linger: 30})
    :ok = :socket.setopt(socket, {:tcp, :nodelay}, true)
    :ok = :socket.bind(socket, %{family: :inet, port: port, addr: addr})
    :ok = :socket.listen(socket, opts[:backlog] || 1024)

    acceptors = :ets.new(:acceptors, [:private, :set])
    state = state(socket: socket, acceptors: acceptors, timeouts: timeouts, plug: plug)
    for _ <- 1..min_acceptors, do: start_acceptor(state)

    {:ok, state}
  end

  @impl true
  def handle_call(:acceptors, _from, state(acceptors: acceptors) = state) do
    {:reply, acceptors, state}
  end

  def handle_call(:socket, _from, state(socket: socket) = state) do
    {:reply, socket, state}
  end

  @impl true
  def handle_cast(:accepted, state) do
    start_acceptor(state)
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, _pid, {:error, :emfile}}, state) do
    Logger.error("Hx could not accept connection: too many open files")
    {:stop, :emfile, state}
  end

  def handle_info({:EXIT, pid, :normal}, state) do
    remove_acceptor(state, pid)
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    Logger.error("Hx Acceptor (pid #{inspect(pid)}) crashed:\n" <> Exception.format_exit(reason))
    remove_acceptor(state, pid)
    {:noreply, state}
  end

  defp remove_acceptor(state(acceptors: acceptors), pid) do
    :ets.delete(acceptors, pid)
  end

  defp start_acceptor(state) do
    state(socket: socket, acceptors: acceptors, timeouts: timeouts, plug: plug) = state
    pid = :proc_lib.spawn_link(__MODULE__, :accept, [self(), socket, timeouts, plug])
    :ets.insert(acceptors, {pid})
  end

  @doc false
  def accept(parent, listen_socket, timeouts, plug) do
    case :socket.accept(listen_socket, timeouts(timeouts, :accept)) do
      {:ok, socket} ->
        GenServer.cast(parent, :accepted)
        keepalive(socket, _buffer = <<>>, timeouts, plug)

      {:error, :timeout} ->
        accept(parent, listen_socket, timeouts, plug)

      {:error, :econnaborted} ->
        accept(parent, listen_socket, timeouts, plug)

      {:error, :closed} ->
        :ok

      {:error, _reason} = error ->
        exit(error)
    end
  end

  Record.defrecordp(:req, [
    :socket,
    :buffer,
    :transfer_encoding,
    :content_length,
    :connection,
    :version,
    :req_headers
  ])

  defp keepalive(socket, buffer, timeouts, plug) do
    case handle_request(socket, buffer, timeouts, plug) do
      req(connection: "close") -> :socket.close(socket)
      req(buffer: buffer) -> keepalive(socket, buffer, timeouts, plug)
    end
  end

  defp handle_request(socket, buffer, timeouts, plug) do
    timeouts(request: request_timeout, headers: headers_timeout) = timeouts

    {method, raw_path, version, buffer, req} = recv_request(socket, buffer, request_timeout)
    {req_headers, buffer, host, req} = recv_headers(socket, version, buffer, headers_timeout, req)
    conn = conn(socket, method, raw_path, req_headers, buffer, host, req)

    %{adapter: {_, req}} = plug.call(conn, _todo_opts = [])

    receive do
      {:plug_conn, :sent} -> :ok
    after
      0 -> :ok
    end

    req
  end

  defp recv_request(socket, buffer, timeout) when byte_size(buffer) <= 1_000 do
    # TODO PicoHTTPParser.parse_request(buffer)
    case :erlang.decode_packet(:http_bin, buffer, []) do
      {:more, _} ->
        case :socket.recv(socket, 0, timeout) |> IO.inspect(label: "recv_request") do
          {:ok, data} ->
            recv_request(socket, buffer <> data, timeout)

          {:error, _reason} ->
            :socket.close(socket)
            exit(:normal)
        end

      {:ok, {:http_request, method, raw_path, version}, buffer} ->
        {method, raw_path, version, buffer, req(version: version)}

      {:ok, {:http_error, _}, _} ->
        send_bad_request(socket)
        :socket.close(socket)
        exit(:normal)

      {:ok, {:http_response, _, _, _}, _} ->
        :socket.close(socket)
        exit(:normal)
    end
  end

  defp recv_request(socket, _buffer, _timeout) do
    send_bad_request(socket)
    :socket.close(socket)
    exit(:normal)
  end

  defp send_bad_request(socket) do
    :socket.send(socket, "HTTP/1.1 400 Bad Request\r\ncontent-length: 11\r\n\r\nBad Request")
  end

  defp recv_headers(socket, {1, _}, buffer, timeout, req) do
    recv_headers(socket, buffer, _headers = [], _count = 0, timeout, _host = nil, req)
  end

  defp recv_headers(socket, {0, 9}, _buffer, _timeout, _req) do
    send_bad_request(socket)
    :socket.close(socket)
    exit(:normal)
  end

  defp recv_headers(socket, buffer, headers, count, timeout, host, req)
       when byte_size(buffer) <= 1_000 and count < 100 do
    case :erlang.decode_packet(:httph_bin, buffer, []) do
      {:ok, {:http_header, _, k, k_bin, v}, buffer} ->
        case k do
          :"Transfer-Encoding" ->
            v = String.downcase(v)
            req = req(req, transfer_encoding: v)
            headers = [{"transfer-encoding", v} | headers]
            recv_headers(socket, buffer, headers, count + 1, timeout, host, req)

          :Connection ->
            v = String.downcase(v)
            req = req(req, connection: v)
            headers = [{"connection", v} | headers]
            recv_headers(socket, buffer, headers, count + 1, timeout, host, req)

          :Host ->
            headers = [{"host", v} | headers]
            recv_headers(socket, buffer, headers, count + 1, timeout, v, req)

          :"Content-Length" ->
            i = String.to_integer(v)
            req = req(req, content_length: i)
            headers = [{"content-length", v} | headers]
            recv_headers(socket, buffer, headers, count + 1, timeout, host, req)

          _ ->
            headers = [{header(k, k_bin), v} | headers]
            recv_headers(socket, buffer, headers, count + 1, timeout, host, req)
        end

      {:ok, :http_eoh, buffer} ->
        {headers, buffer, host, req}

      {:ok, {:http_error, _}, buffer} ->
        recv_headers(socket, buffer, headers, count, timeout, host, req)

      {:more, _} ->
        case :socket.recv(socket, timeout) do
          {:ok, data} ->
            recv_headers(socket, buffer <> data, headers, count, timeout, host, req)

          {:error, _reason} ->
            :socket.close(socket)
            exit(:normal)
        end
    end
  end

  defp recv_headers(socket, _buffer, _headers, _count, _timeout, _host, _req) do
    send_bad_request(socket)
    :socket.close(socket)
    exit(:normal)
  end

  headers = [
    :"Cache-Control",
    # :Connection,
    :Date,
    :Pragma,
    # :"Transfer-Encoding",
    :Upgrade,
    :Via,
    :Accept,
    :"Accept-Charset",
    :"Accept-Encoding",
    :"Accept-Language",
    :Authorization,
    :From,
    # :Host,
    :"If-Modified-Since",
    :"If-Match",
    :"If-None-Match",
    :"If-Range",
    :"If-Unmodified-Since",
    :"Max-Forwards",
    :"Proxy-Authorization",
    :Range,
    :Referer,
    :"User-Agent",
    :Age,
    :Location,
    :"Proxy-Authenticate",
    :Public,
    :"Retry-After",
    :Server,
    :Vary,
    :Warning,
    :"Www-Authenticate",
    :Allow,
    :"Content-Base",
    :"Content-Encoding",
    :"Content-Language",
    # :"Content-Length",
    :"Content-Location",
    :"Content-Md5",
    :"Content-Range",
    :"Content-Type",
    :Etag,
    :Expires,
    :"Last-Modified",
    :"Accept-Ranges",
    :"Set-Cookie",
    :"Set-Cookie2",
    :"X-Forwarded-For",
    :Cookie,
    :"Keep-Alive",
    :"Proxy-Connection"
  ]

  for h <- headers do
    defp header(unquote(h), _), do: unquote(String.downcase(to_string(h)))
  end

  defp header(_, h), do: String.downcase(h)

  defp conn(socket, method, raw_path, req_headers, buffer, host, req) do
    path =
      case raw_path do
        {:abs_path, path} ->
          path

        {:absoluteURI, _scheme, _host, _port, path} ->
          path

        _other ->
          send_bad_request(socket)
          exit(:normal)
      end

    {path, path_info, query_string} =
      case :binary.split(path, "?") do
        [path] -> {path, split_path(path), ""}
        [path, query_string] -> {path, split_path(path), query_string}
      end

    # TODO https://www.erlang.org/doc/man/inet.html#type-returned_non_ip_address
    {:ok, %{addr: remote_ip, port: port}} = :socket.peername(socket)
    req = req(req, socket: socket, buffer: buffer, req_headers: req_headers)

    %Plug.Conn{
      adapter: {__MODULE__, req},
      host: host,
      port: port,
      remote_ip: remote_ip,
      query_string: query_string,
      req_headers: req_headers,
      request_path: path,
      scheme: :http,
      method: Atom.to_string(method),
      path_info: path_info,
      owner: self()
    }
  end

  defp split_path(path) do
    path |> :binary.split("/", [:global]) |> clean_segments()
  end

  @compile inline: [clean_segments: 1]
  defp clean_segments(["" | rest]), do: clean_segments(rest)
  defp clean_segments([segment | rest]), do: [segment | clean_segments(rest)]
  defp clean_segments([] = done), do: done

  @behaviour Plug.Conn.Adapter

  @impl true
  def send_resp(req, status, headers, body) do
    req(socket: socket) = req

    response = [
      http_line(status),
      encode_headers([ensure_content_length(headers, body) | headers]) | body
    ]

    :socket.send(socket, response)

    case List.keyfind(headers, "connection", 0) do
      {_, connection} -> {:ok, nil, req(req, connection: connection)}
      nil -> {:ok, nil, req}
    end
  end

  @impl true
  def send_file(_req, _status, _headers, _file, _offset, _length) do
    raise "not implemented"
  end

  @impl true
  def send_chunked(_req, _status, _headers) do
    raise "not implemented"
  end

  @impl true
  def chunk(_req, _body) do
    {:error, :not_supported}
  end

  @impl true
  def read_req_body(req, opts) do
    req(
      socket: socket,
      buffer: buffer,
      content_length: content_length,
      transfer_encoding: transfer_encoding
    ) = req

    max_length = Keyword.get(opts, :length, 8_000_000)
    # TODO support slow clients
    # read_length = Keyword.get(opts, :read_length, 1_000_000)
    read_timeout = Keyword.get(opts, :read_timeout, 15_000)

    if transfer_encoding && !content_length do
      raise "todo"
    end

    # TODO
    # maybe_send_continue(socket, req_headers)

    buffer_size = byte_size(buffer)

    cond do
      content_length == 0 ->
        {:ok, <<>>, req}

      content_length <= buffer_size ->
        <<body::size(content_length)-bytes, rest::bytes>> = buffer
        {:ok, body, req(req, buffer: rest, content_length: 0)}

      true ->
        left_length = content_length - buffer_size
        recv_length = min(left_length, max_length)

        case :socket.recv(socket, recv_length, read_timeout) do
          {:ok, rest} ->
            tag = if left_length > max_length, do: :more, else: :ok
            {tag, buffer <> rest, req(req, buffer: <<>>, content_length: left_length)}

          {:error, {reason, _data}} ->
            {:error, reason}

          {:error, _reason} = error ->
            error
        end
    end
  end

  # defp maybe_send_continue(socket, req_headers) do
  #   with {_, "100-continue"} <- List.keyfind(req_headers, "expect", 0) do
  #     :socket.send(socket, "HTTP/1.1 100 Continue\r\ncontent-length: 0\r\n\r\n")
  #   end
  # end

  @impl true
  def push(_req, _path, _headers) do
    {:error, :not_supported}
  end

  @impl true
  def inform(_req, _status, _headers) do
    {:error, :not_supported}
  end

  @impl true
  def upgrade(_req, _protocol, _opts) do
    {:error, :not_supported}
  end

  @impl true
  def get_peer_data(req(socket: socket)) do
    {:ok, {address, port}} = :inet.peername(socket)
    %{address: address, port: port, ssl_cert: nil}
  end

  @impl true
  def get_http_protocol(req(version: version)) do
    case version do
      {1, 1} -> :"HTTP/1.1"
      {1, 0} -> :"HTTP/1"
      {0, 9} -> :"HTTP/0.9"
    end
  end

  defp ensure_content_length(headers, body) do
    case List.keyfind(headers, "content-length", 0) do
      nil -> {"content-length", IO.iodata_length(body)}
      {_, _} -> []
    end
  end

  @dialyzer {:no_improper_lists, encode_headers: 1}
  defp encode_headers([{k, v} | rest]) do
    [encode_value(k), ": ", encode_value(v), "\r\n" | encode_headers(rest)]
  end

  defp encode_headers([[] | rest]), do: encode_headers(rest)
  defp encode_headers([]), do: "\r\n"

  defp encode_value(i) when is_integer(i), do: Integer.to_string(i)
  defp encode_value(b) when is_binary(b), do: b

  statuses = [
    {200, "200 OK"},
    {404, "404 Not Found"},
    {500, "500 Internal Server Error"},
    {400, "400 Bad Request"},
    {401, "401 Unauthorized"},
    {403, "403 Forbidden"},
    {429, "429 Too Many Requests"},
    {201, "201 Created"},
    # --------------------
    {100, "100 Continue"},
    {101, "101 Switching Protocols"},
    {102, "102 Processing"},
    {202, "202 Accepted"},
    {203, "203 Non-Authoritative Information"},
    {204, "204 No Content"},
    {205, "205 Reset Content"},
    {206, "206 Partial Content"},
    {207, "207 Multi-Status"},
    {226, "226 IM Used"},
    {300, "300 Multiple Choices"},
    {301, "301 Moved Permanently"},
    {302, "302 Found"},
    {303, "303 See Other"},
    {304, "304 Not Modified"},
    {305, "305 Use Proxy"},
    {306, "306 Switch Proxy"},
    {307, "307 Temporary Redirect"},
    {402, "402 Payment Required"},
    {405, "405 Method Not Allowed"},
    {406, "406 Not Acceptable"},
    {407, "407 Proxy Authentication Required"},
    {408, "408 Request Timeout"},
    {409, "409 Conflict"},
    {410, "410 Gone"},
    {411, "411 Length Required"},
    {412, "412 Precondition Failed"},
    {413, "413 Request Entity Too Large"},
    {414, "414 Request-URI Too Long"},
    {415, "415 Unsupported Media Type"},
    {416, "416 Requested Range Not Satisfiable"},
    {417, "417 Expectation Failed"},
    {418, "418 I'm a teapot"},
    {422, "422 Unprocessable Entity"},
    {423, "423 Locked"},
    {424, "424 Failed Dependency"},
    {425, "425 Unordered Collection"},
    {426, "426 Upgrade Required"},
    {428, "428 Precondition Required"},
    {431, "431 Request Header Fields Too Large"},
    {501, "501 Not Implemented"},
    {502, "502 Bad Gateway"},
    {503, "503 Service Unavailable"},
    {504, "504 Gateway Timeout"},
    {505, "505 HTTP Version Not Supported"},
    {506, "506 Variant Also Negotiates"},
    {507, "507 Insufficient Storage"},
    {510, "510 Not Extended"},
    {511, "511 Network Authentication Required"}
  ]

  for {code, status} <- statuses do
    def status(unquote(code)), do: unquote(status)
    defp http_line(unquote(code)), do: unquote("HTTP/1.1 " <> status <> "\r\n")
  end
end

defmodule Hx do
  @moduledoc "A minimal HTTP/1 server."
  use GenServer
  require Logger
  require Record
  Record.defrecordp(:timeouts, [:accept, :request, :headers])
  Record.defrecordp(:state, [:socket, :acceptors, :timeouts, :handler])

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

    handler = Keyword.fetch!(opts, :handler)
    addr = Keyword.get(opts, :addr, {127, 0, 0, 1})
    port = Keyword.get(opts, :port, 0)
    backlog = Keyword.get(opts, :backlog, 1024)
    min_acceptors = Keyword.get(opts, :acceptors, 100)
    timeouts = Keyword.get(opts, :timeouts, [])

    timeouts =
      timeouts(
        accept: Keyword.get(timeouts, :accept, :timer.seconds(10)),
        request: Keyword.get(timeouts, :request, :timer.seconds(10)),
        headers: Keyword.get(timeouts, :headers, :timer.seconds(10))
      )

    with {:ok, socket} <- :socket.open(:inet, :stream, :tcp),
         :ok <- :socket.setopt(socket, {:socket, :reuseaddr}, true),
         :ok <- :socket.setopt(socket, {:socket, :linger}, %{onoff: true, linger: 30}),
         :ok <- :socket.setopt(socket, {:tcp, :nodelay}, true),
         :ok <- :socket.bind(socket, %{family: :inet, port: port, addr: addr}),
         :ok <- :socket.listen(socket, backlog) do
      acceptors = :ets.new(:acceptors, [:private, :set])
      state = state(socket: socket, acceptors: acceptors, timeouts: timeouts, handler: handler)
      for _ <- 1..min_acceptors, do: start_acceptor(state)
      {:ok, state}
    end
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
  def handle_info({:EXIT, _pid, {:error, :emfile = reason}}, state) do
    Logger.error("Hx could not accept connection: too many open files. Shutting down.")
    {:stop, reason, state}
  end

  def handle_info({:EXIT, pid, :normal}, state) do
    remove_acceptor(state, pid)
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    # TODO
    Logger.error("Hx acceptor (pid #{inspect(pid)}) crashed:\n" <> Exception.format_exit(reason))
    remove_acceptor(state, pid)
    {:noreply, state}
  end

  defp remove_acceptor(state(acceptors: acceptors), pid) do
    :ets.delete(acceptors, pid)
  end

  defp start_acceptor(state) do
    state(socket: socket, acceptors: acceptors, timeouts: timeouts, handler: handler) = state
    pid = :proc_lib.spawn_link(__MODULE__, :accept, [self(), socket, timeouts, handler])
    :ets.insert(acceptors, {pid})
  end

  @doc false
  def accept(parent, listen_socket, timeouts, handler) do
    case :socket.accept(listen_socket, timeouts(timeouts, :accept)) do
      {:ok, socket} ->
        GenServer.cast(parent, :accepted)
        keepalive(socket, _buffer = <<>>, timeouts, handler)

      {:error, :timeout} ->
        accept(parent, listen_socket, timeouts, handler)

      {:error, :econnaborted} ->
        accept(parent, listen_socket, timeouts, handler)

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
    :headers
  ])

  defp keepalive(socket, buffer, timeouts, handler) do
    case handle_request(socket, buffer, timeouts, handler) do
      req(connection: "close") -> :socket.close(socket)
      req(buffer: buffer) -> keepalive(socket, buffer, timeouts, handler)
    end
  end

  @doc false
  def handle_request(socket, buffer, timeouts, handler) do
    timeouts(request: timeout) = timeouts

    {method, url, version, headers, buffer} = recv_request(socket, buffer, timeout)
    {_host, transfer_encoding, content_length, connection, headers} = process_headers(headers)

    # TODO separate url into host and path
    # use https://github.com/ada-url/ada
    # {path, path_info, query_string} =
    #   case :binary.split(url, "?") do
    #     [path] -> {path, split_path(path), ""}
    #     [path, query_string] -> {path, split_path(path), query_string}
    #   end

    # TODO https://www.erlang.org/doc/man/inet.html#type-returned_non_ip_address
    # {:ok, %{addr: remote_ip, port: port}} = :socket.peername(socket)

    req =
      req(
        socket: socket,
        buffer: buffer,
        transfer_encoding: transfer_encoding,
        content_length: content_length,
        connection: connection,
        version: version,
        headers: headers
      )

    # handler.call(method, url, headers, req)
  end

  defp recv_request(socket, <<>>, timeout) do
    case :socket.recv(socket, 0, timeout) do
      {:ok, data} ->
        recv_request(socket, data, timeout)

      {:error, _reason} ->
        :socket.close(socket)
        exit(:normal)
    end
  end

  defp recv_request(socket, buffer, timeout) when byte_size(buffer) <= 2_000 do
    case parse_request(buffer) do
      {_method, _url, _v, _headers, _rest} = req ->
        req

      _pret ->
        case :socket.recv(socket, 0, timeout) do
          {:ok, data} ->
            recv_request(socket, buffer <> data, timeout)

          {:error, _reason} ->
            :socket.close(socket)
            exit(:normal)
        end
    end
  end

  defp parse_request("GET / HTTP/1.1\r\nhost: localhost:60212\r\nuser-agent: mint/1.6.2\r\n\r\n") do
    {"GET", "/", 1, [{"host", "localhost:60212"}, {"user-agent", "mint/1.6.2"}], ""}
  end

  defp recv_request(socket, _buffer, _timeout) do
    send_bad_request(socket)
    :socket.close(socket)
    exit(:normal)
  end

  defp send_bad_request(socket) do
    :socket.send(socket, "HTTP/1.1 400 Bad Request\r\ncontent-length: 11\r\n\r\nBad Request")
  end

  defp process_headers(headers) do
    process_headers(
      headers,
      _host = nil,
      _transfer_encoding = nil,
      _content_length = 0,
      _connection = nil,
      []
    )
  end

  defp process_headers([{k, v} | rest], host, transfer_encoding, content_length, connection, acc) do
    # k = String.downcase(k)
    acc = [{k, v} | acc]

    case k do
      "host" ->
        process_headers(rest, v, transfer_encoding, content_length, connection, acc)

      "transfer-encoding" ->
        process_headers(rest, host, v, content_length, connection, acc)

      "content-length" ->
        process_headers(rest, host, transfer_encoding, String.to_integer(v), connection, acc)

      "connection" ->
        process_headers(rest, host, transfer_encoding, content_length, v, acc)

      _ ->
        process_headers(rest, host, transfer_encoding, content_length, connection, acc)
    end
  end

  defp process_headers([], host, transfer_encoding, content_length, connection, acc) do
    # TODO :lists.reverse?
    {host, transfer_encoding, content_length, connection, acc}
  end

  # TODO URI.decode_query?
  # defp split_path(path) do
  #   path |> :binary.split("/", [:global]) |> clean_segments()
  # end

  # @compile inline: [clean_segments: 1]
  # defp clean_segments(["" | rest]), do: clean_segments(rest)
  # defp clean_segments([segment | rest]), do: [segment | clean_segments(rest)]
  # defp clean_segments([] = done), do: done

  def send_resp(req, status, headers, body) do
    req(socket: socket) = req

    response = [
      http_line(status),
      # TODO
      encode_headers([ensure_content_length(headers, body) | headers])
      | body
    ]

    :socket.send(socket, response)

    # TODO
    case List.keyfind(headers, "connection", 0) do
      {_, connection} -> req(req, connection: connection)
      nil -> req
    end
  end

  # @impl true
  # def send_file(_req, _status, _headers, _file, _offset, _length) do
  #   raise "not implemented"
  # end

  # @impl true
  # def send_chunked(_req, _status, _headers) do
  #   raise "not implemented"
  # end

  # @impl true
  # def chunk(_req, _body) do
  #   {:error, :not_supported}
  # end

  # @impl true
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

  # @impl true
  # def push(_req, _path, _headers) do
  #   {:error, :not_supported}
  # end

  # @impl true
  # def inform(_req, _status, _headers) do
  #   {:error, :not_supported}
  # end

  # @impl true
  # def upgrade(_req, _protocol, _opts) do
  #   {:error, :not_supported}
  # end

  # @impl true
  # def get_peer_data(req(socket: socket)) do
  #   {:ok, {address, port}} = :inet.peername(socket)
  #   %{address: address, port: port, ssl_cert: nil}
  # end

  # @impl true
  # def get_http_protocol(req(version: version)) do
  #   case version do
  #     {1, 1} -> :"HTTP/1.1"
  #     {1, 0} -> :"HTTP/1"
  #     {0, 9} -> :"HTTP/0.9"
  #   end
  # end

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

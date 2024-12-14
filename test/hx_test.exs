defmodule HxTest do
  use ExUnit.Case, async: true

  @finch __MODULE__.Finch

  setup_all do
    {:ok, _finch} = Finch.start_link(name: @finch)

    :ok
  end

  defmodule HelloWorld do
    def call("GET", _path, _headers, req) do
      # _body = Hx.read_body(req)
      Hx.send_resp(
        req,
        _status = 200,
        _headers = [
          {"cache-control", "max-age=0, private, must-revalidate"},
          {"content-type", "text/plain; charset=utf-8"}
        ],
        _body = "Hello, world!"
      )
    end
  end

  test "GET /" do
    {:ok, hx} = Hx.start_link(port: 0, handler: HelloWorld)
    {:ok, %{port: port}} = Hx.sockname(hx)

    assert get("http://localhost:#{port}/") == %Finch.Response{
             status: 200,
             body: "Hello, world!",
             headers: [
               {"content-length", "13"},
               {"cache-control", "max-age=0, private, must-revalidate"},
               {"content-type", "text/plain; charset=utf-8"}
             ]
           }
  end

  defp get(url) do
    req = Finch.build(:get, url)
    Finch.request!(req, @finch)
  end
end

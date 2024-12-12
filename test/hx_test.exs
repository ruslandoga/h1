defmodule HxTest do
  use ExUnit.Case, async: true

  @finch __MODULE__.Finch

  setup_all do
    {:ok, _finch} = Finch.start_link(name: @finch)

    :ok
  end

  defmodule App do
    import Plug.Conn

    def init(opts), do: opts

    def call(conn, _opts) do
      conn
      |> put_resp_content_type("text/plain")
      |> send_resp(200, "Hello, world!")
    end
  end

  test "GET /" do
    {:ok, hx} = Hx.start_link(port: 0, plug: App)
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

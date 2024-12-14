Incomplete but fast HTTP/1.1 & WebSocket web server.

I'm not recommending it but it does work.

```elixir
defmodule HelloWorld do
  import Plug.Conn

  def init(opts), do: opts

  def call(conn, _opts) do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Hello, world!")
  end
end
```

```elixir
Plug.Cowboy.http(HelloWorld, [])
```

```console
$ wrk -d 10 -t 10 -c 100 http://localhost:4000
Running 10s test @ http://localhost:4000
  10 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   766.01us  816.73us  28.20ms   96.95%
    Req/Sec    13.82k     0.87k   15.54k    94.36%
  1388339 requests in 10.10s, 262.18MB read
Requests/sec: 137454.16
Transfer/sec:     25.96MB
```

```elixir
Bandit.start_link(plug: HelloWorld, port: 8000)
```

```console
$ wrk -d 10 -t 10 -c 100 http://localhost:8000
Running 10s test @ http://localhost:8000
  10 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   845.09us    1.35ms  32.45ms   93.82%
    Req/Sec    16.66k     2.67k   30.11k    73.84%
  1672925 requests in 10.10s, 327.06MB read
Requests/sec: 165637.84
Transfer/sec:     32.38MB
```

```elixir
defmodule HelloWorld do
  def call("GET", _path, _headers, req) do
    # _body = Hx.read_body(req)
    Hx.send_resp(req, _status = 200, _headers = [{"content-type", "text/plain"}], _body = "Hello, world!")
  end
end

Hx.start_link(port: 8000, handler: HelloWorld)
```

```console
$ wrk -d 10 -t 10 -c 100 http://localhost:8000
Running 10s test @ http://localhost:8000
  10 threads and 100 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   839.70us    1.58ms  66.86ms   93.52%
    Req/Sec    19.36k     1.76k   25.20k    69.50%
  1945368 requests in 10.10s, 144.71MB read
Requests/sec: 192597.57
Transfer/sec:     14.33MB
```

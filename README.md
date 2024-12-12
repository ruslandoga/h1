Basic but fast HTTP/1.1 web server.

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
$ wrk -d 10 -t 10 -c 10 http://localhost:4000
Running 10s test @ http://localhost:4000
  10 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   234.86us  751.93us  26.03ms   99.75%
    Req/Sec     4.89k   274.67     6.27k    89.21%
  491026 requests in 10.10s, 92.73MB read
Requests/sec:  48617.36
Transfer/sec:      9.18MB 
```

```elixir
Hx.start_link(port: 8000, plug: HelloWorld)
```

```console
$ wrk -d 10 -t 10 -c 10 http://localhost:8000
Running 10s test @ http://localhost:8000
  10 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   102.05us   31.13us   1.10ms   73.74%
    Req/Sec     9.55k   578.27    10.38k    88.02%
  959342 requests in 10.10s, 132.66MB read
Requests/sec:  94989.51
Transfer/sec:     13.14MB
```

```elixir
Bandit.start_link(plug: HelloWorld, port: 8000)
```

```console
$ wrk -d 10 -t 10 -c 10 http://localhost:8000
Running 10s test @ http://localhost:8000
  10 threads and 10 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency   173.47us  361.99us  16.15ms   99.80%
    Req/Sec     6.12k   400.12     7.83k    84.36%
  614518 requests in 10.10s, 120.14MB read
Requests/sec:  60844.28
Transfer/sec:     11.90MB
```

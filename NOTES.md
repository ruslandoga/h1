Hm, maybe no Plug?

```elixir
defmodule A do
  @already_sent {:plug_conn, :sent}
  
  def do_send_return(i) do
    result = i + i
    send(self(), @already_sent)

    receive do
      @already_sent -> :ok
    after
      0 -> :ok
    end

    result
  end

  def do_return(i) do
    i + i
  end
end

:timer.tc(fn -> Enum.each(1..10000, &A.do_return/1) end)
# 115ms avg after three runs
:timer.tc(fn -> Enum.each(1..10000, &A.do_send_return/1) end)
# 2387ms avg after three runs
```

---

the api should just be a function

```elixir
Hx.http(fn "GET", _url, _headers, req ->
  [] = Hx.body(req) # might be no-op
  {200, [{"content-type", "text/plain"}, {"cache-control"}], "hello, world!"}
end)
```

I only know of interfaces provided by Plug, Cowboy, and Elli. For inspiration, might need to check out others, like Raxx, Ace, Aino, Mar, Nova, 

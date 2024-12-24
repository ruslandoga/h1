Hm, maybe no Plug?

---

the api should just be a function

```elixir
pid =
  Hx.http(fn "GET", _url, _headers, req ->
    [] = Hx.body(req) # might be no-op
    {200, [{"content-type", "text/plain"}, {"cache-control"}], "hello, world!"}
  end)

IO.inspect(Hx.sockname(pid), label: "sockname")
```

... and `hx` would expose lots and lots of helpers to make writing this function eaiser.

I only know of interfaces provided by Plug, Cowboy, and Elli. For inspiration, might need to check out others, like Raxx, Ace, Aino, Mar, Nova, 

Hm, maybe no Plug?

---

the api should just be a function

```elixir
Hx.http(fn "GET", _url, _headers, req ->
  [] = Hx.body(req) # might be no-op
  {200, [{"content-type", "text/plain"}, {"cache-control"}], "hello, world!"}
end)
```

I only know of interfaces provided by Plug, Cowboy, and Elli. For inspiration, might need to check out others, like Raxx, Ace, Aino, Mar, Nova, 

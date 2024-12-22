Benchee.run(
  %{
    "wip" => fn input ->
      Hx.handle_request(_socket = nil, input, {:timeouts, nil, nil, nil}, _handler = nil)
    end
  },
  profile_after: true,
  inputs: %{
    "GET /" => "GET / HTTP/1.1\r\nhost: localhost:60212\r\nuser-agent: mint/1.6.2\r\n\r\n"
  }
)

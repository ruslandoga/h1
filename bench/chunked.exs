defmodule Bench do
  def spawn_blackhole do
    {:ok, listen_socket} = :gen_tcp.listen(0, ip: {127, 0, 0, 1}, mode: :binary, active: false)
    {:ok, {addr, port}} = :inet.sockname(listen_socket)
    pid = :proc_lib.spawn_link(__MODULE__, :accept, [listen_socket])
    {addr, port, pid}
  end

  @doc false
  def accept(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket, :infinity)
    loop_discard(socket)
  end

  defp loop_discard(socket) do
    case :gen_tcp.recv(socket, 0, :infinity) do
      {:ok, _data} -> loop_discard(socket)
      {:error, _} -> :done
    end
  end

  def imperative(socket, chunks) do
    Process.put(__MODULE__, :sent)
    Enum.each(chunks, fn chunk -> :gen_tcp.send(socket, chunk) end)
  end

  def send_self(socket, chunks) do
    Enum.each(chunks, fn chunk ->
      send(self(), {:chunk, chunk})

      receive do
        {:chunk, chunk} -> :gen_tcp.send(socket, chunk)
      end
    end)
  end

  def collectable_stream(socket, chunks) do
    chunks |> Stream.into(Chunked.new(socket)) |> Stream.run()
  end
end

Benchee.run(
  %{
    "imperative" => fn %{socket: socket, chunks: chunks} ->
      Bench.imperative(socket, chunks)
    end,
    "collectable_stream" => fn %{socket: socket, chunks: chunks} ->
      Bench.collectable_stream(socket, chunks)
    end,
    "send_self" => fn %{socket: socket, chunks: chunks} ->
      Bench.send_self(socket, chunks)
    end
  },
  before_scenario: fn chunks ->
    {addr, port, _pid} = Bench.spawn_blackhole()
    {:ok, socket} = :gen_tcp.connect(addr, port, mode: :binary, active: false)
    %{chunks: chunks, socket: socket}
  end,
  profile_after: true,
  inputs: %{
    "a few" => Enum.map(1..10, fn n -> String.duplicate("a", n * 100) end)
  }
)

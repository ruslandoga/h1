defmodule Chunked do
  defstruct [:socket]

  def new(socket) do
    %__MODULE__{socket: socket}
  end

  @doc false
  def collector(socket, op) do
    case op do
      {:cont, chunk} -> :gen_tcp.send(socket, chunk)
      :done -> :ok
      :halt -> :ok
    end

    socket
  end
end

defimpl Collectable, for: Chunked do
  def into(chunked) do
    {chunked.socket, &Chunked.collector/2}
  end
end

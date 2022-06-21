defmodule Challenge.Operator do
  use GenServer

  def start_link(_) do
    GenServer.start(__MODULE__, :ok, [])
  end

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end
end

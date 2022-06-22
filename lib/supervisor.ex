defmodule Challenge.Supervisor do
  use Supervisor

  def start_link(_opts) do
    Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    children = [
      {Challenge.Mnesia.Server, []},
      {DynamicSupervisor, strategy: :one_for_one, name: Challenge.DynamicSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end
end

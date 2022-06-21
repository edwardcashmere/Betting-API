defmodule Challenge do
  @moduledoc false
  use Application

  def start(_, _) do
    Challenge.Supervisor.start_link([])
  end

  @spec start :: GenServer.server()
  def start do
    DynamicSupervisor.start_child(Challenge.DynamicSupervisor, {Challenge.Operator, []})
  end
end

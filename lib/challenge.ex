defmodule Challenge do
  @moduledoc false
  use Application

  @bet "bet"
  @win "win"
  @type bet_or_win :: %{
          user: String.t(),
          status: String.t(),
          request_uuid: binary(),
          currency: String.t(),
          balance: number()
        }

  def start(_, _) do
    Challenge.Supervisor.start_link([])
  end

  @spec start :: GenServer.server()
  def start do
    {:ok, pid} =
      DynamicSupervisor.start_child(Challenge.DynamicSupervisor, {Challenge.Operator, []})

    pid
  end

  @spec create_users(server_pid :: GenServer.server(), list(String.t())) :: :ok
  def create_users(server_pid, users) do
    Challenge.Operator.add_users(server_pid, users)
  end

  @spec bet(server_pid :: GenServer.server(), body :: map()) :: bet_or_win()
  def bet(server_pid, body) do
    task = fn -> Challenge.Operator.process_bet(server_pid, body) end

    task
    |> Task.async()
    |> Task.await(:infinity)
  end

  @spec win(server_pid :: GenServer.server(), body :: map()) :: bet_or_win()
  def win(server_pid, body) do
    task = fn -> Challenge.Operator.process_win(server_pid, body) end

    task
    |> Task.async()
    |> Task.await(:infinity)
  end

  # def send_request(request_type, body) do
  #   pid = spawn (fn -> bet_win_request() end)
  #   IO.inspect(pid, label: "handler process")
  #   {:ok, pid} = DynamicSupervisor.start_child(Challenge.DynamicSupervisor, {Challenge.BetWinServer, [pid,request_type, body]}) |> IO.inspect(label: "supervisor pid")
  #   pid
  # end

  # def bet_win_request() do
  #   receive do
  #     {:response, response} ->
  #          response
  #   end
  # end
end

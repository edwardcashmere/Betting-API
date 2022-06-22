defmodule Challenge.Operator do
  use GenServer

  alias Challenge.Mnesia.Server

  @type bet_or_win :: %{
          user: String.t(),
          status: String.t(),
          request_uuid: binary(),
          currency: String.t(),
          balance: number()
        }

  def start_link(_) do
    GenServer.start(__MODULE__, :ok, [])
  end

  @spec add_users(server :: GenServer.server(), users :: [String.t()]) :: :ok
  def add_users(server, users) do
    GenServer.cast(server, {:create_users, users})
  end

  # @spec bet(server_pid :: GenServer.server(), map()) :: bet_or_win
  # def bet(server_pid, body) do
  #   GenServer.call(server_pid, {:bet, body})
  # end

  # def win(server_pid, GenServer)

  # def bet do

  # end

  # def win do

  # end

  @impl true
  def init(:ok) do
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:create_users, users}, state) do
    :ok = Enum.each(users, fn user -> Server.create_user(user) end)
    {:noreply, state}
  end
end

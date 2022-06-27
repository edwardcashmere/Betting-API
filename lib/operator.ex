defmodule Challenge.Operator do
  use GenServer

  alias Challenge.Mnesia.Server
  alias Challenge.BetWin

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

  @spec process_bet(server_pid :: GenServer.server(), map()) :: bet_or_win
  def process_bet(server_pid, body) do
    GenServer.call(server_pid, {:bet, body})
  end

  @spec process_win(server_pid :: GenServer.server(), body :: map()) :: bet_or_win
  def process_win(server_pid, body) do
    GenServer.call(server_pid, {:win, body})
  end

  @impl true
  def init(:ok) do
    Process.flag(:trap_exit, true)
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:create_users, users}, state) do
    users
    |> Enum.filter(fn user -> is_binary(user) end)
    |> Enum.each(fn user ->
      user
      |> String.trim()
      |> Server.create_user()
    end)

    {:noreply, state}
  end

  @impl true
  def handle_call({:win, body}, from, state) do
    # check that the trans_id does not exist in mnesia win was not prcoess before
    # maybe check the reference trans_id exist for bet associated with this win
    # then get the user on request
    # add win to amount and update user
    # send response back to caller
    DynamicSupervisor.start_child(
      Challenge.DynamicSupervisor,
      {BetWin, [self(), from, "win", body]}
    )

    {:noreply, state}
  end

  @impl true
  def handle_call({:bet, body}, from, state) do
    # check that the trans_id does not exist in mnesia bet was not prcoess before otherwise return early
    # then get the user on request
    # confirm they have the amount to make the bet other return early
    # deduct amount from user and update user from mnesia
    # send response back to caller
    DynamicSupervisor.start_child(
      Challenge.DynamicSupervisor,
      {BetWin, [self(), from, "bet", body]}
    )

    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, _pid, :normal}, state) do
    # {:ok, _pid} = ThirdPartyModule.start_link(config)
    {:noreply, state}
  end

  @impl true
  def handle_info({:EXIT, _pid, _reason}, state) do
    # :timer.sleep(retry_delay)
    # {:ok, _pid} = ThirdPartyModule.start_link(config)
    {:noreply, state}
  end

  # defp build_response(user, status, new_amount, request_id) do
  #   {:ok,
  #    %{
  #      user: user,
  #      status: status,
  #      currency: "USD",
  #      balance: new_amount,
  #      request_uuid: request_id
  #    }}
  # end

  @spec calculate_new_amount(amount :: number(), name :: String.t(), bet_win :: number()) ::
          {:ok, number()}
  def calculate_new_amount(amount, name, bet_win) do
    case name do
      "bet" ->
        {:ok, amount - absolute_value(bet_win)}

      "win" ->
        {:ok, amount + absolute_value(bet_win)}
    end
  end

  # defp check_amount(user_amount, bet) do
  #   {:ok, user_amount >= bet}
  # end

  @spec absolute_value(amount :: number()) :: number()
  def absolute_value(amount) when amount > 0 do
    amount
  end

  def absolute_value(amount) do
    -1 * amount
  end
end

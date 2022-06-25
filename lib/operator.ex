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
    spawn_link(fn ->
      # check that the trans_id does not exist in mnesia win was not prcoess before
      # maybe check the reference trans_id exist for bet associated with this win
      # then get the user on request
      # add win to amount and update user
      # send response back to caller
      IO.puts("Processing.......")

      with false <- check_trans_id?(body[:transaction_uuid], "win"),
           {:ok, name, amount, _currency} <- get_user(body[:user]),
           {:ok, new_amount} <- calculate_new_amount(amount, body[:amount]),
           :ok <- update_user(name, new_amount),
           :ok <- update_transactions_table(body[:transaction_uuid], "win", true),
           {:ok, response} <- build_response(name, "RS_OK", new_amount) do
        IO.puts("I got a response back")

        GenServer.reply(from, response)
      else
        true ->
          case get_user(body[:user]) do
            {:ok, name, amount, _currency} ->
              {:ok, response} = build_response(name, "RS_ERROR_DUPLICATE_TRANSACTION", amount)
              GenServer.reply(from, response)

            _ ->
              {:ok, response} = build_response("UKNOWN", "RS_ERROR_DUPLICATE_TRANSACTION", 0)
              GenServer.reply(from, response)
          end

        :does_not_exit ->
          {:ok, response} = build_response("UKNOWN", "RS_ERROR_INVALID_PARTNER", 0)
          GenServer.reply(from, response)

        _ ->
          {:ok, name, amount, _currency} = get_user(body[:user])
          {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount)
          GenServer.reply(from, response)
      end
    end)

    {:noreply, state}
  end

  @impl true
  def handle_call({:bet, body}, from, state) do
    spawn_link(fn ->
      # check that the trans_id does not exist in mnesia bet was not prcoess before otherwise return early
      # then get the user on request
      # confirm they have the amount to make the bet other return early
      # deduct amount from user and update user from mnesia
      # send response back to caller
      IO.puts("Processing.......")

      with false <- check_trans_id?(body[:transaction_uuid], "bet"),
           {:ok, name, amount, _currency} <- get_user(body[:user]),
           {:ok, true} <- check_amount(amount, body[:amount]),
           {:ok, new_amount} <- calculate_new_amount(amount, -body[:amount]),
           :ok <- update_user(name, new_amount),
           :ok <- update_transactions_table(body[:transaction_uuid], "bet", true),
           {:ok, response} <- build_response(name, "RS_OK", new_amount) do
        IO.puts("I got a response back")
        GenServer.reply(from, response)
      else
        true ->
          case get_user(body[:user]) do
            {:ok, name, amount, _currency} ->
              {:ok, response} = build_response(name, "RS_ERROR_DUPLICATE_TRANSACTION", amount)
              GenServer.reply(from, response)

            _ ->
              {:ok, response} = build_response("UKNOWN", "RS_ERROR_DUPLICATE_TRANSACTION", 0)
              GenServer.reply(from, response)
          end

        :does_not_exit ->
          {:ok, response} = build_response("UKNOWN", "RS_ERROR_INVALID_PARTNER", 0)
          GenServer.reply(from, response)

        {:ok, false} ->
          {:ok, name, amount, _currency} = get_user(body[:user])
          {:ok, response} = build_response(name, "RS_ERROR_NOT_ENOUGH_MONEY", amount)
          GenServer.reply(from, response)

        _ ->
          {:ok, name, amount, _currency} = get_user(body[:user])
          {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount)
          GenServer.reply(from, response)
      end
    end)

    {:noreply, state}
  end

  @spec get_user(name :: String.t()) ::
          {:ok, name :: String.t(), amount :: number(), currency :: String.t()}
          | :error
          | :does_not_exit

  def get_user(name) do
    Server.get_user(name)
  end

  @spec check_trans_id?(trans_id :: binary(), request_type :: String.t()) :: boolean() | :error
  def check_trans_id?(trans_id, request_type) do
    Server.transaction_id_exist?(trans_id, request_type)
  end

  @spec update_transactions_table(trans_id :: binary(), name :: String.t(), status :: boolean()) ::
          :ok | :error
  def update_transactions_table(trans_id, name, status) do
    Server.add_transaction_uuid(trans_id, name, status)
  end

  @spec update_user(name :: String.t(), amount :: String.t()) ::
          :failed_to_update_user | :error | :ok
  def update_user(name, amount) do
    Server.update_user(name, amount)
  end

  defp build_response(user, status, new_amount) do
    {:ok,
     %{
       user: user,
       status: status,
       currency: "USD",
       balance: new_amount
     }}
  end

  defp calculate_new_amount(amount, bet_win) do
    IO.inspect(amount + bet_win, label: "this is a bet or win")
    IO.inspect(bet_win, label: "this is a bet or win")
    {:ok, amount + bet_win}
  end

  defp check_amount(user_amount, bet) do
    {:ok, user_amount >= bet}
  end
end

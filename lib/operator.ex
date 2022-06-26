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
      body = body

      with :transaction_found <-
             Server.get_transaction_id(body[:reference_transaction_uuid], "bet"),
           :transaction_does_not_exist <-
             Server.get_transaction_id(body[:transaction_uuid], "win"),
           {:ok, name, amount, _currency} <- Server.get_user(body[:user]),
           {:ok, new_amount} <-
             calculate_new_amount(amount, "win", body[:amount]),
           :ok <- Server.update_user(name, new_amount),
           :ok <- Server.add_transaction_uuid(body[:transaction_uuid], "win", true),
           {:ok, response} <- build_response(name, "RS_OK", new_amount, body[:request_uuid]) do
        GenServer.reply(from, response)
      else
        :transaction_found ->
          case Server.get_user(body[:user]) do
            {:ok, name, amount, _currency} ->
              {:ok, response} =
                build_response(
                  name,
                  "RS_ERROR_DUPLICATE_TRANSACTION",
                  amount,
                  body[:request_uuid]
                )

              GenServer.reply(from, response)

            _ ->
              {:ok, response} =
                build_response("UNKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])

              GenServer.reply(from, response)
          end

        :transaction_does_not_exist ->
          case Server.get_user(body[:user]) do
            {:ok, name, amount, _currency} ->
              {:ok, response} =
                build_response(
                  name,
                  "RS_ERROR_TRANSACTION_DOES_NOT_EXIST",
                  amount,
                  body[:request_uuid]
                )

              GenServer.reply(from, response)

            _ ->
              {:ok, response} =
                build_response("UNKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])

              GenServer.reply(from, response)
          end

        :does_not_exist ->
          {:ok, response} = build_response("UNKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])
          GenServer.reply(from, response)

        _ ->
          {:ok, name, amount, _currency} = Server.get_user(body[:user])
          {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount, body[:request_uuid])
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

      body = body

      with :transaction_does_not_exist <-
             Server.get_transaction_id(body[:transaction_uuid], "bet"),
           {:ok, name, amount, _currency} <- Server.get_user(body[:user]),
           {:ok, true} <- check_amount(amount, body[:amount]),
           {:ok, new_amount} <- calculate_new_amount(amount, "bet", body[:amount]),
           :ok <- Server.update_user(name, new_amount),
           :ok <- Server.add_transaction_uuid(body[:transaction_uuid], "bet", true),
           {:ok, response} <- build_response(name, "RS_OK", new_amount, body[:request_uuid]) do
        GenServer.reply(from, response)
      else
        :transaction_found ->
          case Server.get_user(body[:user]) do
            {:ok, name, amount, _currency} ->
              {:ok, response} =
                build_response(
                  name,
                  "RS_ERROR_DUPLICATE_TRANSACTION",
                  amount,
                  body[:request_uuid]
                )

              GenServer.reply(from, response)

            _ ->
              {:ok, response} =
                build_response("UKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])

              GenServer.reply(from, response)
          end

        :does_not_exist ->
          {:ok, response} = build_response("UNKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])
          GenServer.reply(from, response)

        {:ok, false} ->
          {:ok, name, amount, _currency} = Server.get_user(body[:user])

          {:ok, response} =
            build_response(name, "RS_ERROR_NOT_ENOUGH_MONEY", amount, body[:request_uuid])

          GenServer.reply(from, response)

        _ ->
          {:ok, name, amount, _currency} = Server.get_user(body[:user])
          {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount, body[:request_uuid])
          GenServer.reply(from, response)
      end
    end)

    {:noreply, state}
  end

  defp build_response(user, status, new_amount, request_id) do
    {:ok,
     %{
       user: user,
       status: status,
       currency: "USD",
       balance: new_amount,
       request_uuid: request_id
     }}
  end

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

  defp check_amount(user_amount, bet) do
    {:ok, user_amount >= bet}
  end

  @spec absolute_value(amount :: number()) :: number()
  def absolute_value(amount) when amount > 0 do
    amount
  end

  def absolute_value(amount) do
    -1 * amount
  end

  :math
end

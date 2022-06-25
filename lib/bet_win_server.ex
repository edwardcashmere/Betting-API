defmodule Challenge.BetWinServer do
  use GenServer, restart: :transient

  alias Challenge.Mnesia.Server

  def start_link([caller, request_type, opts]) do
    GenServer.start_link(__MODULE__, %{caller: caller, request_type: request_type, opts: opts},
      name: :"#{request_type}#{opts[:transaction_uuid]}"
    )
  end

  @impl true
  def init(%{opts: opts, request_type: request_type, caller: caller}) do
    send(self(), {String.to_atom(request_type), opts})
    {:ok, %{opts: opts, caller: caller}}
  end

  @impl true
  def handle_info({:bet, body}, %{caller: caller} = state) do
    # check that the trans_id does not exist in mnesia bet was not prcoess before otherwise return early
    # then get the user on request
    # confirm they have the amount to make the bet other return early
    # deduct amount from user and update user from mnesia
    # send response back to caller

    with false <- check_trans_id?(body[:transaction_uuid], "bet"),
         {:ok, name, amount, _currency} <- get_user(body[:user]),
         {:ok, true} <- check_amount(amount, body[:amount]),
         {:ok, new_amount} <- calculate_new_amount(amount, -body[:amount]),
         :ok <- update_user(name, new_amount),
         :ok <- update_transactions_table(body[:transaction_uuid], "bet", true),
         {:ok, response} <- build_response(name, "RS_OK", new_amount) do
      send(caller, {:response, response})
      exit(:normal)
    else
      true ->
        case get_user(body[:user]) do
          {:ok, name, amount, _currency} ->
            {:ok, response} = build_response(name, "RS_ERROR_DUPLICATE_TRANSACTION", amount)
            send(caller, {:response, response})
            exit(:normal)

          _ ->
            {:ok, response} = build_response("UKNOWN", "RS_ERROR_DUPLICATE_TRANSACTION", 0)
            send(caller, {:response, response})
            exit(:normal)
        end

      :does_not_exit ->
        {:ok, response} = build_response("UKNOWN", "RS_ERROR_INVALID_PARTNER", 0)
        send(caller, {:response, response})
        exit(:normal)

      {:ok, false} ->
        {:ok, name, amount, _currency} = get_user(body[:user])
        {:ok, response} = build_response(name, "RS_ERROR_NOT_ENOUGH_MONEY", amount)
        send(caller, {:response, response})
        exit(:normal)

      _ ->
        {:ok, name, amount, _currency} = get_user(body[:user])
        {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount)
        send(caller, {:response, response})
        exit(:normal)
    end

    {:noreply, state}
  end

  def handle_info({:win, body}, %{caller: caller} = state) do
    # check that the trans_id does not exist in mnesia win was not prcoess before
    # then get the user on request
    # add win to amount and update user
    # send response back to caller

    with false <- check_trans_id?(body[:transaction_uuid], "win"),
         {:ok, name, amount, _currency} <- get_user(body[:user]),
         {:ok, new_amount} <- calculate_new_amount(amount, body[:amount]),
         :ok <- update_user(name, new_amount),
         :ok <- update_transactions_table(body[:transaction_uuid], "win", true),
         {:ok, response} <- build_response(name, "RS_OK", new_amount) do

      send(caller, {:response, response})
      exit(:normal)
    else
      true ->
        case get_user(body[:user]) do
          {:ok, name, amount, _currency} ->
            {:ok, response} = build_response(name, "RS_ERROR_DUPLICATE_TRANSACTION", amount)
            send(caller, {:response, response})
            exit(:normal)

          _ ->
            {:ok, response} = build_response("UKNOWN", "RS_ERROR_DUPLICATE_TRANSACTION", 0)
            send(caller, {:response, response})
            exit(:normal)
        end

      :does_not_exit ->
        {:ok, response} = build_response("UKNOWN", "RS_ERROR_INVALID_PARTNER", 0)
        send(caller, {:response, response})
        exit(:normal)

      _ ->
        {:ok, name, amount, _currency} = get_user(body[:user])
        {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount)
        send(caller, {:response, response})
        exit(:normal)
    end

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
    {:ok, amount + bet_win}
  end

  defp check_amount(user_amount, bet) do
    {:ok, user_amount >= bet}
  end
end

defmodule Challenge.BetWin do
  use GenServer, restart: :transient

  alias Challenge.Mnesia.Server
  alias Challenge.Operator

  require Logger

  @retry_number 3
  @timeout 1000

  def start_link([_operator_pid, _request_type, _from, body] = opts) do
    GenServer.start_link(__MODULE__, opts, name: :"request_type#{body[:transaction_uuid]}")
  end

  @impl true
  def init([operator_pid, from, request_type, body]) do
    Process.link(operator_pid)
    Process.flag(:trap_exit, true)
    send(self(), :process_request)
    {:ok, %{request_type: request_type, from: from, body: body, retries: 0}}
  end

  def handle_info(:process_request, %{request_type: request_type, from: from, body: body} = state) do
    case request_type do
      "bet" ->
        process_bet(from, body)

        {:stop, :normal, state}

      "win" ->
        process_win(from, body)
        {:stop, :normal, state}
    end
  end

  @impl true
  def handle_info(:retry, %{retries: retries, from: from, body: body} = state) do
    if retries <= @retry_number do
      Process.send_after(self(), :process_request, @timeout)
    else
      {:ok, name, amount, _currency} = Server.get_user(body[:user])
      {:ok, response} = build_response(name, "RS_ERROR_UNKNOWN", amount, body[:request_uuid])
      GenServer.reply(from, response)

      {:stop, :normal, state}
    end
  end

  defp process_win(from, body) do
    with :transaction_found <-
           Server.get_transaction_id(body[:reference_transaction_uuid], "bet", true),
         :transaction_does_not_exist <-
           Server.get_transaction_id(body[:transaction_uuid], "win", true),
         {:ok, name, amount, _currency} <- Server.get_user(body[:user]),
         {:ok, new_amount} <-
           Operator.calculate_new_amount(amount, "win", body[:amount]),
         :ok <- Server.add_transaction_uuid(body[:transaction_uuid], "win", true),
         :ok <- Server.update_user(name, new_amount),
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
        send(self(), :retry)
    end
  end

  defp process_bet(from, body) do
    with :transaction_does_not_exist <-
           Server.get_transaction_id(body[:transaction_uuid], "bet", true),
         {:ok, name, amount, _currency} <- Server.get_user(body[:user]),
         {:ok, true} <- check_amount(amount, body[:amount]),
         {:ok, new_amount} <- Operator.calculate_new_amount(amount, "bet", body[:amount]),
         :ok <- Server.add_transaction_uuid(body[:transaction_uuid], "bet", true),
         :ok <- Server.update_user(name, new_amount),
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
            {:ok, response} = build_response("UKNOWN", "RS_ERROR_UNKNOWN", 0, body[:request_uuid])

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

        send(self(), :retry)
    end
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

  defp check_amount(user_amount, bet) do
    {:ok, user_amount >= bet}
  end
end

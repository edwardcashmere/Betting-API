defmodule Challenge.Mnesia.Server do
  alias :mnesia, as: Mnesia

  use GenServer

  require Logger

  @currency "USD"

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  @impl true
  def init(_opts) do
    Mnesia.create_schema([Node.self()])
    Mnesia.start()
    send(self(), :create_tables)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:create_tables, state) do
    create_tables()
    Mnesia.add_table_index(Challenge.Bet_Win, :status)
    Mnesia.add_table_index(Challenge.Bet_Win, :name)
    {:noreply, state}
  end

  def create_tables() do
    add_table(Challenge.User, attributes: [:name, :amount, :currency])
    # add status
    add_table(Challenge.Bet_Win, attributes: [:transaction_bet_uuid, :name, :status])
  end

  @spec create_user(name :: String.t(), amount :: number()) :: :ok
  def create_user(name, amount \\ 100_00)

  def create_user(name, amount) when byte_size(name) > 0 do
    t_fn = fn -> Mnesia.write({Challenge.User, name, amount, @currency}) end

    with :does_not_exist <- get_user(name),
         {:ok, _result} <- run_transaction(t_fn) do
      IO.puts("user created")
      :ok
    else
      {:ok, _name, _amount, _currency} ->
        :ok

      {:error, _error} ->
        :ok
    end
  end

  def create_user(_, _) do
    :ok
  end

  @doc """
  adding successfull transaction_ids from bets or wins
  to table
  """
  @spec add_transaction_uuid(trans_uuid :: binary(), name :: binary(), status :: boolean()) :: :ok | :error
  def add_transaction_uuid(trans_uuid, name, status) do
    t_fn = fn -> Mnesia.write({Challenge.Bet_Win, trans_uuid, name, status}) end

    case run_transaction(t_fn) do
      {:ok, results} ->
        IO.inspect(results)
        :ok

      {:error, _error} ->
        :error
    end
  end

  @spec transaction_id_exist?(trans_uuid :: binary(), name :: binary()) :: boolean() | :error
  def transaction_id_exist?(trans_uuid, name) do
    t_fn = fn -> Mnesia.match_object({Challenge.Bet_Win, trans_uuid, name, :_}) end

    case run_transaction(t_fn) do
      {:ok, [{Challenge.Bet_Win, ^trans_uuid, ^name, _status}]} -> true
      {:ok, []} -> false
      {:error, _reason} -> :error
    end
  end

  @spec get_all_users :: :no_users_found | :error | any()
  def get_all_users() do
    t_fn = fn -> Mnesia.match_object({Challenge.User, :_, :_, :_}) end

    case run_transaction(t_fn) do
      {:ok, []} -> :no_users_found
      {:ok, results} -> IO.inspect(results)
      {:error, _reason} -> :error
    end
  end

  @spec get_user(name :: String.t()) ::
          {:ok, name :: String.t(), amount :: number(), currency :: String.t()}
          | :error
          | :does_not_exit
  def get_user(name) do
    t_fn = fn -> Mnesia.read(Challenge.User, name) end

    case run_transaction(t_fn)  do
      {:ok, [{_table_name, name, amount, currency}]} -> {:ok, name, amount, currency}
      {:ok, []} -> :does_not_exist
      {:error, _reason} -> :error
    end
  end

  @spec update_user(name :: String.t(), amount :: number()) :: :failed_to_update_user | :error | :ok
  def update_user(name, amount) do
    t_fn = fn -> Mnesia.write({Challenge.User, name, amount, @currency}) end

    case run_transaction(t_fn) do
      {:ok, [{_table_name, _name, _amount, _currency}]} -> :ok
      {:ok, []} -> :failed_to_update_user
      {:error, _error} -> :error
    end

  end

  @spec get_all_trans_ids_with_criteria() :: [{binary(), String.t(), boolean()}]
  def get_all_trans_ids_with_criteria() do
    t_fn = fn -> Mnesia.match_object({Challenge.Bet_Win, :_, :_, :_}) end

    case run_transaction(t_fn) do
      {:ok, []} -> :no_transaction_ids_found
      {:ok, results} -> IO.inspect(results)
      {:error, _reason} -> :error
    end

  end

  defp run_transaction(t_fn) do
    case Mnesia.transaction(t_fn) do
      {:atomic, result} ->
        {:ok, result}

      {:aborted, reason} ->
        {:error, reason}
    end
  end

  defp add_table(name, attrs) do
    case Mnesia.create_table(name, attrs) |> IO.inspect() do
      {:atomic, :ok} ->
        IO.puts("Table created")
        :ok

      {:aborted, {:already_exists, _}} ->
        :ok

      {:aborted, {:already_exists, _, _}} ->
        :ok

      _ ->
        :error
    end
  end

end

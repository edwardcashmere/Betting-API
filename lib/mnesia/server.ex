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
  def create_user(name, amount \\ 100_000)

  def create_user(name, amount) when byte_size(name) > 0 do
    t_fn = fn -> Mnesia.write({Challenge.User, name, amount, @currency}) end

    with :does_not_exist <- get_user(name),
         {:ok, _result} <- run_transaction(t_fn) do
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
  @spec add_transaction_uuid(trans_uuid :: binary(), name :: binary(), status :: boolean()) ::
          :ok | :error
  def add_transaction_uuid(trans_uuid, name, status) do
    t_fn = fn -> Mnesia.write({Challenge.Bet_Win, trans_uuid, name, status}) end

    case run_transaction(t_fn) do
      {:ok, _results} ->
        :ok

      {:error, _error} ->
        :error
    end
  end

  @spec get_transaction_id(trans_uuid :: binary(), name :: binary(), status :: boolean()) ::
          :transaction_found | :transaction_does_not_exist | :error
  def get_transaction_id(trans_uuid, name, status) do
    t_fn = fn -> Mnesia.match_object({Challenge.Bet_Win, trans_uuid, name, status}) end

    case run_transaction(t_fn) do
      {:ok, [{Challenge.Bet_Win, ^trans_uuid, ^name, _status}]} -> :transaction_found
      {:ok, []} -> :transaction_does_not_exist
      {:error, _reason} -> :error
    end
  end

  @spec get_all_users ::
          :no_users_found
          | :error
          | [{String.t(), number(), String.t()}]
  def get_all_users() do
    t_fn = fn -> Mnesia.match_object({Challenge.User, :_, :_, :_}) end

    case run_transaction(t_fn) do
      {:ok, []} -> :no_users_found
      {:ok, results} -> results
      {:error, _reason} -> :error
    end
  end

  @spec get_user(name :: String.t()) ::
          {:ok, String.t(), number(), String.t()}
          | :error
          | :does_not_exit
  def get_user(name) do
    t_fn = fn -> Mnesia.read(Challenge.User, name) end

    case run_transaction(t_fn) do
      {:ok, [{_table_name, name, amount, currency}]} -> {:ok, name, amount, currency}
      {:ok, []} -> :does_not_exist
      {:error, _reason} -> :error
    end
  end

  @spec update_user(name :: String.t(), amount :: number()) :: :ok | :error
  def update_user(name, amount) do
    t_fn = fn -> Mnesia.write({Challenge.User, name, amount, @currency}) end

    case run_transaction(t_fn) do
      {:ok, _result} -> :ok
      {:error, _error} -> :error
    end
  end

  @spec get_all_trans_ids_with_criteria(name :: any(), status :: any()) :: [
          {binary(), String.t(), boolean()}
        ]
  def get_all_trans_ids_with_criteria(name \\ :_, status \\ :_) do
    t_fn = fn -> Mnesia.match_object({Challenge.Bet_Win, :_, name, status}) end

    case run_transaction(t_fn) do
      {:ok, []} -> :no_transaction_ids_found
      {:ok, results} -> results
      {:error, _reason} -> :error
    end
  end

  def dummy_func() do
    Mnesia.system_info()
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
    case Mnesia.create_table(name, attrs) do
      {:atomic, :ok} ->
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

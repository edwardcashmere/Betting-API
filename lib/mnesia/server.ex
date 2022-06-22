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
    send(self(), :create_table)
    {:ok, %{}}
  end

  @impl true
  def handle_info(:create_table, state) do
    case Mnesia.create_table(Challenge.User, attributes: [:name, :amount, :currency])
         |> IO.inspect() do
      {:atomic, :ok} ->
        IO.puts("table created")
        {:noreply, state}

      {:aborted, {:already_exists, _, _}} ->
        Logger.info("Failed to create ,mnsesia table")
        {:noreply, state}

      {:aborted, {:already_exists, _}} ->
        IO.puts("Failed to create")
        IO.inspect(Mnesia.system_info(:tables))
        {:noreply, state}

      _ ->
        raise "Failed to create mnesia tables something went wrong"
    end
  end

  @spec create_user(name :: String.t(), amount :: number()) :: :ok
  def create_user(name, amount \\ 100_00)

  def create_user(name, amount) when is_binary(name) and not is_nil(name) do
    t_fn = fn -> Mnesia.write({Challenge.User, name, amount, @currency}) end

    with :does_not_exist <- get_user(name) |> IO.inspect(),
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

  @spec get_all_users :: :no_users_found | :error | any()
  def get_all_users() do
    t_fn = fn -> Mnesia.match_object({Challenge.User, :_, :_, :_}) end

    case run_transaction(t_fn) do
      {:ok, []} -> :no_users_found
      {:ok, results} -> IO.inspect(results)
      {:error, _error} -> :error
    end
  end

  @spec get_user(name :: String.t()) ::
          {:ok, name :: String.t(), amount :: number(), currency :: String.t()}
          | :error
          | :does_not_exit
  def get_user(name) do
    t_fn = fn -> Mnesia.read(Challenge.User, name) end

    case run_transaction(t_fn) |> IO.inspect() do
      {:ok, [{_table_name, name, amount, currency}]} -> {:ok, name, amount, currency}
      {:ok, []} -> :does_not_exist
      error -> {:error, error}
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
end

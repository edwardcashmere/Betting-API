defmodule ChallengeTest do
  use ExUnit.Case
  doctest Challenge

  alias Challenge.Mnesia.Server

  setup do
    pid = Challenge.start()
    %{server_pid: pid, currency: "USD", amount: 100_000}
  end

  test "test create users works with an list of users", %{
    server_pid: pid,
    currency: _currency,
    amount: _amount
  } do
    users = ["Messi", "Lebron", "Cristiano", "Curry"]
    assert :ok = Challenge.create_users(pid, users)
    result = Server.get_all_users()
    IO.inspect(result, label: "results")
    # {:ok, "Messi", amount, currency} = Server.get_user("Messi)
    # {:ok, "Lebron", amount, currency} = Server.get_user("Lebron"))
    # {:ok, "Cristiano", amount, currency} = Server.get_user("Curry")
  end

  @tag :skip
  test "create users ingores empty string names and non-binary values from list", %{
    server_pid: pid
  } do
    users = ["Kobe", "", " ", 56, :durant, "Hamilton", %{}, []]
    assert :ok = Challenge.create_users(pid, users)
  end

  @tag :skip
  test "existing names in system are not overwritten byb new values that match the same name", %{
    server_pid: pid
  } do
    users = ["Kobe", "Jordan"]
    Challenge.create_users(pid, users)
    Server.create_user("Jordan", 200_000)
    Server.create_user("Kobe", 200_000)

    assert {:ok, "Kobe", 100_000, currency} = Server.get_user("Kobe")
    assert {:ok, "Jordan", 100_000, currency} = Server.get_user("Kobe")
  end
end

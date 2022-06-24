defmodule ChallengeTest do
  use ExUnit.Case
  doctest Challenge

  alias Challenge.Mnesia.Server

  @test   %{
    user: "john12345",
    transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
    supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
    token: "55b7518e-b89e-11e7-81be-58404eea6d16",
    supplier_user: "cg_45141",
    round_closed: true,
    round: "rNEMwgzJAOZ6eR3V",
    reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
    request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
    is_free: false,
    is_aggregated: false,
    game_code: "clt_dragonrising",
    currency: "EUR",
    bet: "zero",
    amount: 100000,
    meta: %{
      selection: "home_team",
      odds: 2.5
    }
  }

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


  @tag :skip
  test "bet", %{server_pid: pid} do
    data = %{
      "user": "john12345",
      "transaction_uuid": "16d2dcfe-b89e-11e7-854a-58404eea6d16",
      "supplier_transaction_id": "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
      "token": "55b7518e-b89e-11e7-81be-58404eea6d16",
      "supplier_user": "cg_45141",
      "round_closed": true,
      "round": "rNEMwgzJAOZ6eR3V",
      "reward_uuid": "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
      "request_uuid": "583c985f-fee6-4c0e-bbf5-308aad6265af",
      "is_free": false,
      "is_aggregated": false,
      "game_code": "clt_dragonrising",
      "currency": "EUR",
      "bet": "zero",
      "amount": 100500,
      "meta": %{
        "selection": "home_team",
        "odds": 2.5
      }
    }

    Challenge.bet(pid, data)
  end


end

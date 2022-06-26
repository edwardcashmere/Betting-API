defmodule ChallengeTest do
  use ExUnit.Case
  doctest Challenge

  alias :mnesia, as: Mnesia

  alias Challenge.Mnesia.Server
  alias Challenge.Operator

  setup do
    pid = Challenge.start()
    Mnesia.clear_table(Challenge.User)
    Mnesia.clear_table(Challenge.Bet_Win)

    %{server_pid: pid, currency: "USD", amount: 100_000}
  end

  setup_all do
    on_exit(fn ->
      Mnesia.stop()
      Mnesia.delete_schema(Node)
    end)
  end

  test "create users works with an list of users", %{
    server_pid: pid,
    currency: currency,
    amount: amount
  } do
    users = ["Messi", "Lebron", "Cristiano", "Curry"]

    Challenge.create_users(pid, users)
    Process.sleep(1)
    assert {:ok, "Messi", ^amount, ^currency} = Server.get_user("Messi")
    assert {:ok, "Lebron", ^amount, ^currency} = Server.get_user("Lebron")
    assert {:ok, "Cristiano", ^amount, ^currency} = Server.get_user("Cristiano")
  end

  test "create users ingores empty string names and non-binary values from list", %{
    server_pid: pid
  } do
    users = ["Kobe", "", " ", 56, :durant, "Hamilton", %{}, []]
    assert :ok = Challenge.create_users(pid, users)
    Process.sleep(1)
    users = Server.get_all_users()
    assert length(users) == 2
  end

  test "existing names in system are not overwritten by new values that match the same name", %{
    server_pid: pid,
    currency: currency,
    amount: amount
  } do
    users = ["Kobe", "Jordan"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    Server.create_user("Jordan", 200_000)
    Server.create_user("Kobe", 200_000)

    assert {:ok, "Kobe", ^amount, ^currency} = Server.get_user("Kobe")
    assert {:ok, "Jordan", ^amount, ^currency} = Server.get_user("Jordan")
  end

  test "test absolute function" do
    assert 1000 = Operator.absolute_value(1000)
    assert 1000 = Operator.absolute_value(-1000)
  end

  test "calculate amount" do
    assert {:ok, 10_000} = Operator.calculate_new_amount(100_000, "bet", 90_000)
    assert {:ok, 10_000} = Operator.calculate_new_amount(100_000, "bet", -90_000)
    assert {:ok, 205_000} = Operator.calculate_new_amount(100_000, "win", 105_000)
    assert {:ok, 205_000} = Operator.calculate_new_amount(100_000, "win", -105_000)
  end

  # test bet with valid payload
  # test bet with valid payload -ve amount
  # test bet with duplicate trans_id
  # test bet with user that does not exist
  # test bet with multiple bets concurrently
  test "bet works as expected", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john12345"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:valid_data)

    assert %{
             user: "john12345",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert {:ok, "john12345", 0, ^currency} = Server.get_user("john12345")
  end

  test "a bet can be made with valid data with -ve amount value", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john1234"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:negative_amount_data)

    assert %{
             user: "john1234",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert {:ok, "john1234", 0, ^currency} = Server.get_user("john1234")
  end

  test " user should not be able to bet with an amount greater than what they have in their wallet",
       %{server_pid: pid, currency: currency} do
    users = ["john1"]

    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:invalid_data)

    assert %{
             user: "john1",
             status: "RS_ERROR_NOT_ENOUGH_MONEY",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 100_000
           } = Challenge.bet(pid, bet)

    assert {:ok, "john1", 100_000, ^currency} = Server.get_user("john1")
  end

  test "a bet that was already processed is not processed again", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john12"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:duplicate_trans_id_data)

    assert %{
             user: "john12",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert %{
             user: "john12",
             status: "RS_ERROR_DUPLICATE_TRANSACTION",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert {:ok, "john12", 0, ^currency} = Server.get_user("john12")
  end

  test "a bet cannot be made when the user does not exist", %{server_pid: pid, currency: currency} do
    users = ["john12"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:user_does_not_exist_data)

    assert %{
             user: "UNKNOWN",
             status: "RS_ERROR_UNKNOWN",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)
  end

  # test win with valid payload
  # test win with valid payload -ve amount
  # test win with duplicate trans_id
  # test win with user that does not exist
  # test win with multiple bets concurrently
  test "a win can be made with valid data", %{server_pid: pid, currency: currency} do
    users = ["john12345"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:valid_data)

    win = test_win_data(:valid_data)

    assert %{
             user: "john12345",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert %{
             user: "john12345",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 110_000
           } = Challenge.win(pid, win)

    assert {:ok, "john12345", 110_000, ^currency} = Server.get_user("john12345")
  end

  test "a win can be made with valid data with -ve amount value", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john1234"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:negative_amount_data)

    win = test_win_data(:negative_amount_data)

    assert %{
             user: "john1234",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert %{
             user: "john1234",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 110_000
           } = Challenge.win(pid, win)

    assert {:ok, "john1234", 110_000, ^currency} = Server.get_user("john1234")
  end

  test " a win that does not have a corresponding bet reference-transaction_id should fail", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john12345"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    win = test_win_data(:invalid_data)

    assert %{
             user: "john12345",
             status: "RS_ERROR_TRANSACTION_DOES_NOT_EXIST",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 100_000
           } = Challenge.win(pid, win)

    assert {:ok, "john12345", 100_000, ^currency} = Server.get_user("john12345")
  end

  test "a win that was already processed is not processed again", %{
    server_pid: pid,
    currency: currency
  } do
    users = ["john12"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    bet = test_bet_data(:duplicate_trans_id_data)

    win = test_win_data(:duplicate_trans_id_data)

    assert %{
             user: "john12",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.bet(pid, bet)

    assert %{
             user: "john12",
             status: "RS_OK",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 110_000
           } = Challenge.win(pid, win)

    assert %{
             user: "john12",
             status: "RS_ERROR_DUPLICATE_TRANSACTION",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 110_000
           } = Challenge.win(pid, win)

    assert {:ok, "john12", 110_000, ^currency} = Server.get_user("john12")
  end

  test "a win cannot be made when the user does not exist", %{server_pid: pid, currency: currency} do
    users = ["john12"]
    Challenge.create_users(pid, users)
    Process.sleep(1)

    win = test_win_data(:user_does_not_exist_data)

    assert %{
             user: "UNKNOWN",
             status: "RS_ERROR_UNKNOWN",
             request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
             currency: ^currency,
             balance: 0
           } = Challenge.win(pid, win)
  end

  defp test_bet_data(data_type) do
    case data_type do
      :valid_data ->
        %{
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
          currency: "USD",
          bet: "zero",
          amount: 100_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :invalid_data ->
        %{
          user: "john1",
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
          currency: "USD",
          bet: "zero",
          amount: 100_001,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :negative_amount_data ->
        %{
          user: "john1234",
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
          currency: "USD",
          bet: "zero",
          amount: -100_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :user_does_not_exist_data ->
        %{
          user: "unknown_user",
          transaction_uuid: "16d2dcfe-b89e-11e7-854a-dvndjv322",
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
          currency: "USD",
          bet: "zero",
          amount: 100_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :duplicate_trans_id_data ->
        %{
          user: "john12",
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
          currency: "USD",
          bet: "zero",
          amount: 100_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }
    end
  end

  defp test_win_data(data_type) do
    case data_type do
      :valid_data ->
        %{
          user: "john12345",
          transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
          token: "55b7518e-b89e-11e7-81be-58404eea6d16",
          supplier_user: "cg_45141",
          round_closed: true,
          round: "rNEMwgzJAOZ6eR3V",
          reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
          request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
          reference_transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          is_free: false,
          is_aggregated: false,
          game_code: "clt_dragonrising",
          currency: "EUR",
          bet: "zero",
          amount: 110_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :invalid_data ->
        %{
          user: "john12345",
          transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
          token: "55b7518e-b89e-11e7-81be-58404eea6d16",
          supplier_user: "cg_45141",
          round_closed: true,
          round: "rNEMwgzJAOZ6eR3V",
          reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
          request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
          reference_transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          is_free: false,
          is_aggregated: false,
          game_code: "clt_dragonrising",
          currency: "USD",
          bet: "zero",
          amount: 100_500,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :negative_amount_data ->
        %{
          user: "john1234",
          transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404e234244521",
          supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
          token: "55b7518e-b89e-11e7-81be-58404eea6d16",
          supplier_user: "cg_45141",
          round_closed: true,
          round: "rNEMwgzJAOZ6eR3V",
          reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
          request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
          reference_transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          is_free: false,
          is_aggregated: false,
          game_code: "clt_dragonrising",
          currency: "EUR",
          bet: "zero",
          amount: -110_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :user_does_not_exist_data ->
        %{
          user: "uknown_user",
          transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea23445",
          supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
          token: "55b7518e-b89e-11e7-81be-58404eea6d16",
          supplier_user: "cg_45141",
          round_closed: true,
          round: "rNEMwgzJAOZ6eR3V",
          reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
          request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
          reference_transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          is_free: false,
          is_aggregated: false,
          game_code: "clt_dragonrising",
          currency: "USD",
          bet: "zero",
          amount: 110_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }

      :duplicate_trans_id_data ->
        %{
          user: "john12",
          transaction_uuid: "1111111111111-222222222-3333333333",
          supplier_transaction_id: "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
          token: "55b7518e-b89e-11e7-81be-58404eea6d16",
          supplier_user: "cg_45141",
          round_closed: true,
          round: "rNEMwgzJAOZ6eR3V",
          reward_uuid: "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
          request_uuid: "583c985f-fee6-4c0e-bbf5-308aad6265af",
          reference_transaction_uuid: "16d2dcfe-b89e-11e7-854a-58404eea6d16",
          is_free: false,
          is_aggregated: false,
          game_code: "clt_dragonrising",
          currency: "USD",
          bet: "zero",
          amount: 110_000,
          meta: %{
            selection: "home_team",
            odds: 2.5
          }
        }
    end
  end

  # @tag :skip
  # test "bet", %{server_pid: pid} do
  #   # data = %{
  #   #   "user": "john12345",
  #   #   "transaction_uuid": "16d2dcfe-b89e-11e7-854a-58404eea6d16",
  #   #   "supplier_transaction_id": "41ecc3ad-b181-4235-bf9d-acf0a7ad9730",
  #   #   "token": "55b7518e-b89e-11e7-81be-58404eea6d16",
  #   #   "supplier_user": "cg_45141",
  #   #   "round_closed": true,
  #   #   "round": "rNEMwgzJAOZ6eR3V",
  #   #   "reward_uuid": "a28f93f2-98c5-41f7-8fbb-967985acf8fe",
  #   #   "request_uuid": "583c985f-fee6-4c0e-bbf5-308aad6265af",
  #   #   "is_free": false,
  #   #   "is_aggregated": false,
  #   #   "game_code": "clt_dragonrising",
  #   #   "currency": "EUR",
  #   #   "bet": "zero",
  #   #   "amount": 100500,
  #   #   "meta": %{
  #   #     "selection": "home_team",
  #   #     "odds": 2.5
  #   #   }
  #   # }

  #   # Challenge.bet(pid, data)
  # end
end

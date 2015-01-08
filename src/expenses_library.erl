%%%-------------------------------------------------------------------
%%% @author leandroloureiro
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2014 16:38
%%%-------------------------------------------------------------------
-module(expenses_library).
-author("leandroloureiro").


-include("deps/cqerl/include/cqerl.hrl").

%% API
-export([get_transactions/2, get_accounts/1, get_all_categories/0, get_all_subcategories/0, add_transaction/7, check_account_auth/2]).
-export([get_account_user_id/1]).
-export([get_accounts_aux/1]).
-export([get_account_sum/1]).
-export([add_account/5]).

%% DEV
-export([get_transactions_aux/1]).

get_account_user_id(AccountId) ->
  try
    AccountIdUUID = uuid:string_to_uuid(AccountId),
    case cqerl:new_client() of
      {ok, Client} ->
        case cqerl:run_query(Client, #cql_query{statement = <<"SELECT user_id FROM user_by_account WHERE account_id = ?;">>, values = [{account_id, AccountIdUUID}]}) of
          {ok, Result} ->
            Row = cqerl:head(Result),
            cqerl:close_client(Client),
            case Row of
              empty_dataset ->
                not_found;
              _ ->
                UserId = proplists:get_value(user_id, Row),
                {ok, UserId}
            end;
          _ ->
            cqerl:close_client(Client),
            error
        end;
      _ ->
        system_error
    end
  catch
    exit:badarg -> not_found
  end.

get_transactions(ClientId, AccountId) ->
  case get_account_user_id(AccountId) of
    {ok, ClientId} ->
      get_transactions_aux(AccountId);
    {ok, _} ->
      access_denied;
    _ ->
      system_error
  end.


get_transactions_aux(AccountId) ->
  try
    AccountIdUUID = uuid:string_to_uuid(AccountId),
    case cqerl:new_client() of
      {ok, Client} ->
        case cqerl:run_query(Client, #cql_query{statement = <<"SELECT transaction_id,date,description,amount,category,sub_category,external_reference FROM transactions WHERE account_id = ?;">>, values = [{account_id, AccountIdUUID}]}) of
          {ok, Result} ->
            cqerl:close_client(Client),
            cqerl:all_rows(Result);
          _ ->
            cqerl:close_client(Client),
            system_error
        end;
      _ ->
        system_error
    end
  catch
    exit:badarg -> not_found
  end.


get_accounts(ClientId) ->
  Accounts = get_accounts_aux(ClientId),
  case Accounts of
    [_ | _] ->
      ProcessedBalances = fun(Account) ->
        Balance = get_account_sum(proplists:get_value(account_id, Account)),
        lists:concat([Account, [{balance, Balance}]])
      end,
      lists:map(ProcessedBalances, Accounts);
    [] ->
      [];
    _ ->
      system_error
  end.


get_accounts_aux(ClientId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_id FROM accounts_by_user WHERE user_id = ?">>, values = [{user_id, ClientId}]}) of
        {ok, Result} ->
          cqerl:close_client(Client),
          Accounts = cqerl:all_rows(Result),
          GetAccountsDetailMap = fun(Account) ->
            AccountId = proplists:get_value(account_id, Account),
            AccountInfo = get_account_datail_info(AccountId),
            lists:concat([Account, AccountInfo])
          end,
          lists:map(GetAccountsDetailMap, Accounts);
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.


get_account_datail_info(AccountId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_type,currency,name,start_balance FROM accounts WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
        {ok, Result} ->
          cqerl:close_client(Client),
          cqerl:head(Result);
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.


get_account_sum(AccountId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT amount FROM transactions WHERE account_id = ?">>, values = [{account_id, AccountId}], page_size = 10000}) of
        {ok, Result} ->
          cqerl:close_client(Client),
          Values = cqerl:all_rows(Result),
          sum_transactions_amount(Values);
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.

sum_transactions_amount(Values) ->
  sum_transactions_amount(Values, 0).

sum_transactions_amount([H | T], Acc) ->
  Amount = proplists:get_value(amount, H),
  sum_transactions_amount(T, Amount + Acc);

sum_transactions_amount([], Acc) -> Acc.


get_all_categories() ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, "SELECT name FROM category") of
        {ok, Result} ->
          cqerl:close_client(Client),
          lists:sort(cqerl:all_rows(Result));
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.


get_all_subcategories() ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, "SELECT category_name,name FROM sub_category") of
        {ok, Result} ->
          cqerl:close_client(Client),
          lists:sort(cqerl:all_rows(Result));
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.


add_transaction(AccountId, Description, Category, SubCategory, Amount, Timestamp, Tags) ->
  AccountIdUUID = uuid:string_to_uuid(AccountId),
  TransactionId = uuid:get_v4(strong),
  case cqerl:new_client() of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{
        statement = <<"INSERT INTO transactions
          (transaction_id,date,account_id,amount,description,category,sub_category)
          VALUES (?, ?, ?, ?, ?, ?, ?)">>,
        values = [
          {transaction_id, TransactionId},
          {account_id, AccountIdUUID},
          {description, Description},
          {amount, Amount},
          {date, Timestamp},
          {category, Category},
          {sub_category, SubCategory}
        ]
      }),
      cqerl:close_client(Client),
      case Result of
        {ok, void} ->
          AddTagsMap = fun(Tag) ->
            add_tag_to_transaction(TransactionId, Tag)
          end,
          Results = lists:map(AddTagsMap, Tags),
          ResultOk = fun(SingleResult) -> SingleResult == ok end,
          case lists:all(ResultOk, Results) of
            true ->
              {ok, uuid:uuid_to_string(TransactionId, binary_standard)};
            _ ->
              system_error
          end;
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.

add_tag_to_transaction(TransactionId, Tag) when is_integer(Tag) ->
  add_tag_to_transaction(TransactionId, integer_to_list(Tag));

add_tag_to_transaction(TransactionId, Tag) when is_list(Tag) ->
  case cqerl:new_client() of
    {ok, Client} ->
      Result = cqerl:run_query(Client,
        #cql_query{statement = <<"INSERT INTO tags_by_transaction (transaction_id,tag) VALUES (?,?)">>,
          values = [{transaction_id, TransactionId}, {tag, Tag}]}
      ),
      case Result of
        {ok, void} ->
          Result2 = cqerl:run_query(Client,
            #cql_query{statement = <<"INSERT INTO transactions_by_tag (transaction_id,tag) VALUES (?,?)">>,
              values = [{transaction_id, TransactionId}, {tag, Tag}]}
          ),
          cqerl:close_client(Client),
          case Result2 of
            {ok, void} ->
              ok;
            _ ->
              system_error
          end;
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.

add_account(Name, Type, StartBalance, Currency, UserID) ->
  AccountID = uuid:get_v4(strong),
  case cqerl:new_client() of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{
        statement = <<"INSERT INTO accounts
          (account_id,name,account_type,start_balance,currency,user_id) VALUES (?,?,?,?,?,?)">>,
        values = [
          {account_id, AccountID},
          {name, Name},
          {account_type, Type},
          {start_balance, StartBalance},
          {currency, Currency},
          {user_id, UserID}
        ]}),
      cqerl:close_client(Client),
      case Result of
        {ok, void} ->
          case insert_user_by_account(UserID, AccountID) of
            ok ->
              case insert_account_by_user(AccountID, UserID) of
                ok ->
                  {ok, uuid:uuid_to_string(AccountID, binary_standard)};
                _ ->
                  system_error
              end;
            _ ->
              system_error
          end;
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.

insert_user_by_account(UserId, AccountId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{
        statement = <<"INSERT INTO user_by_account (account_id,user_id) VALUES (?,?)">>,
        values = [
          {account_id, AccountId},
          {user_id, UserId}
        ]}),
      case Result of
        {ok, void} ->
          ok;
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.

insert_account_by_user(AccountId, UserId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{
        statement = <<"INSERT INTO accounts_by_user (user_id,account_id) VALUES (?,?)">>,
        values = [
          {user_id, UserId},
          {account_id, AccountId}
        ]}),
      case Result of
        {ok, void} ->
          ok;
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.

check_account_auth(ClientId, AccountId) ->
  try
    AccountIdUUID = uuid:string_to_uuid(AccountId),
    case cqerl:new_client() of
      {ok, Client} ->
        case cqerl:run_query(Client, #cql_query{statement = <<"SELECT user_id FROM user_by_account WHERE account_id = ?">>, values = [{account_id, AccountIdUUID}]}) of
          {ok, Result} ->
            cqerl:close_client(Client),
            Row = cqerl:head(Result),
            case Row of
              empty_dataset ->
                not_found;
              _ ->
                case proplists:get_value(user_id, Row) of
                  ClientId ->
                    valid;
                  _ ->
                    denied
                end
            end;
          _ ->
            cqerl:close_client(Client),
            system_error
        end;
      _ ->
        system_error
    end
  catch
    exit:badarg -> not_found
  end.








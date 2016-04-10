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


-include_lib("cqerl/include/cqerl.hrl").

-export([get_transactions/2, get_all_categories/0, get_all_subcategories/0, add_transaction/7, check_account_auth/2]).
-export([get_account_user_id/1]).
-export([get_client_accounts/1]).
-export([add_account/5]).
-export([get_account_info/1]).
-export([get_account_transactions_balance/1]).
-export([get_account_transactions/1]).
-export([create_accounts_query/1]).

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
      get_account_transactions(AccountId);
    {ok, _} ->
      access_denied;
    _ ->
      system_error
  end.


get_account_transactions(AccountId) ->
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

get_client_accounts(ClientId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_id FROM accounts_by_user WHERE user_id = ?">>, values = [{user_id, ClientId}]}) of
        {ok, Result} ->
          cqerl:close_client(Client),
          case cqerl:all_rows(Result) of
            [] ->
              [];
            Accounts ->
              GetAccountIds = fun(Account) -> uuid:uuid_to_string(proplists:get_value(account_id, Account)) end,
              get_client_accounts_information(lists:map(GetAccountIds, Accounts))
          end;
        _ ->
          cqerl:close_client(Client),
          system_error
      end;
    _ ->
      system_error
  end.

get_client_accounts_information(AccountIds) ->
  AccountsINClause = list_to_binary("(" ++ create_accounts_query(AccountIds)),
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT * FROM accounts WHERE account_id IN ", AccountsINClause/binary>>}) of
        {ok, Result} ->
          cqerl:all_rows(Result);
        _ ->
          system_error
      end;
    _ ->
      sytem_error
  end.

create_accounts_query([T]) ->
  T ++ ")";

create_accounts_query([H | T]) ->
  H ++ "," ++ create_accounts_query(T).


get_account_info(AccountId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_id, account_type,currency,name,start_balance FROM accounts WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
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


get_account_transactions_balance(AccountId) ->
  case cqerl:new_client() of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"select count(*) as transactions, sum(amount) as balance from transactions WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
        {ok, Result} ->
          cqerl:close_client(Client),
          cqerl:head(Result);
        _ ->
          system_error
      end;
    _ ->
      system_error
  end.


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








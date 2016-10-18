%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2014 16:38
%%%-------------------------------------------------------------------
-module(expenses_library).
-author("leandroloureiro").

-include_lib("cqerl/include/cqerl.hrl").

-export([
  get_all_categories/1,
  get_all_subcategories/1,
  add_category/2,
  add_sub_category/3,
  verify_category/2,
  delete_category/2,
  delete_sub_category/3,
  verify_sub_category/3,

  add_transaction/7,
  delete_transaction/3,
  verify_transaction/3,

  check_account_auth/2,
  get_client_accounts/1,
  add_account/5,
  delete_account/2,
  get_account_info/1,
  get_account_transactions_balance/1,
  get_account_transactions/3
]).

get_account_transactions(AccountId, Start, End) when is_binary(Start) ->
  case string:to_integer(binary_to_list(Start)) of
    {error, _} ->
      lager:log(warning, self(), "Invalid start argument when fetching transactions for account ~s", [AccountId]),
      system_error;
    {StartDate, _} ->
      get_account_transactions(AccountId, StartDate, End)
  end;

get_account_transactions(AccountId, Start, End) when is_binary(End) ->
  case string:to_integer(binary_to_list(End)) of
    {error, _} ->
      lager:log(warning, self(), "Invalid end argument when fetching transactions for account ~s", [AccountId]),
      system_error;
    {EndDate, _} ->
      get_account_transactions(AccountId, Start, EndDate)
  end;

get_account_transactions(AccountId, Start, End) when is_integer(Start), is_integer(End) ->
  try
    AccountIdUUID = uuid:string_to_uuid(AccountId),
    case cqerl:get_client({}) of
      {ok, Client} ->
        case cqerl:run_query(Client, #cql_query{statement = <<"SELECT transaction_id,date,description,amount,category,sub_category,external_reference FROM transactions WHERE account_id = ? AND date > :start_date AND date < :end_date">>, values = [{account_id, AccountIdUUID}, {start_date, Start}, {end_date, End}], page_size = 50000}) of
          {ok, Result} ->
            cqerl:all_rows(Result);
          {error, {Code, Description, _}} ->
            lager:log(error, self(), "Problem when fetching transactions for account ~s: ~B - ~s", [AccountId, Code, Description]),
            system_error
        end;
      _ ->
        system_error
    end
  catch
    exit:badarg -> not_found
  end.

get_client_accounts(ClientId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_id FROM accounts_by_user WHERE user_id = ?">>, values = [{user_id, ClientId}]}) of
        {ok, Result} ->
          case cqerl:all_rows(Result) of
            [] ->
              [];
            Accounts ->
              GetAccountIds = fun(Account) -> uuid:uuid_to_string(proplists:get_value(account_id, Account)) end,
              get_client_accounts_information(lists:map(GetAccountIds, Accounts))
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem when fetching accounts for user ~s: ~B - ~s", [uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

get_client_accounts_information(AccountIds) ->
  AccountsINClause = list_to_binary("(" ++ create_accounts_query(AccountIds)),
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT * FROM accounts WHERE account_id IN ", AccountsINClause/binary>>}) of
        {ok, Result} ->
          cqerl:all_rows(Result);
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem when fetching account information for accounts ~s: ~B - ~s", [AccountsINClause, Code, Description]),
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
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT account_id, account_type,currency,name,start_balance FROM accounts WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
        {ok, Result} ->
          cqerl:head(Result);
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem when getting account information for account ~s: ~B - ~s", [AccountId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


get_account_transactions_balance(AccountId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"select count(*) as transactions, sum(amount) as balance from transactions WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
        {ok, Result} ->
          cqerl:head(Result);
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem getting transactions balance for account ~s: ~B - ~s", [AccountId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


get_all_categories(ClientId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = "SELECT name FROM category WHERE user_id = ?", values = [{user_id, ClientId}]}) of
        {ok, Result} ->
          lists:sort(cqerl:all_rows(Result));
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem getting categories for user ~s: ~B - ~s", [uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

add_category(ClientId, Category) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = "INSERT INTO category (user_id,name) VALUES (?,?)",
        values = [{user_id, ClientId}, {name, Category}]}) of
        {ok, void} ->
          true;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem adding new category ~s for user ~s: ~B - ~s", [Category, uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

get_all_subcategories(ClientId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = "SELECT category_name,name FROM sub_category WHERE user_id = ? ", values = [{user_id, ClientId}]}) of
        {ok, Result} ->
          lists:sort(cqerl:all_rows(Result));
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem fetching all sub categories for user ~s: ~B - ~s", [uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

verify_category(ClientId, Category) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = "SELECT name FROM category WHERE user_id = ? AND name = ? ",
        values = [{user_id, ClientId}, {name, Category}]}),
      case Result of
        {ok, Data} ->
          Categories = cqerl:all_rows(Data),
          case Categories of
            [] ->
              false;
            [_] ->
              true
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem fetching category ~s for user ~s: ~B - ~s", [Category, uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

verify_sub_category(ClientId, Category, SubCategory) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = "SELECT name FROM sub_category WHERE user_id = ? AND category_name = ? AND name = ?",
        values = [{user_id, ClientId}, {category_name, Category}, {name, SubCategory}]}),
      case Result of
        {ok, Data} ->
          Categories = cqerl:all_rows(Data),
          case Categories of
            [] ->
              false;
            [_] ->
              true
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem fetching category ~s for user ~s - ~s: ~B - ~s", [Category, SubCategory, uuid:uuid_to_string(ClientId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

add_sub_category(ClientId, Category, SubCategory) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = <<"INSERT INTO sub_category (user_id, category_name, name) VALUES (?,?,?)">>,
        values = [{user_id, ClientId}, {category_name, Category}, {name, SubCategory}]}),
      case Result of
        {ok, void} ->
          true;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem adding sub category ~s to category ~s for user ~s: ~B - ~s", [SubCategory, Category, uuid:uuid_to_string(ClientId), Code, Description]),
          false
      end;
    _ ->
      system_error
  end.

delete_category(ClientId, Category) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM sub_category WHERE user_id = ? AND category_name = ?">>,
        values = [{user_id, ClientId}, {category_name, Category}]}),
      case Result of
        {ok, void} ->
          Result2 = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM category WHERE user_id = ? AND name = ?">>,
            values = [{user_id, ClientId}, {name, Category}]}),
          case Result2 of
            {ok, void} ->
              true;
            {error, {Code, Description, _}} ->
              lager:log(error, self(), "Problem deleting category ~s for user ~s: ~B - ~s", [Category, uuid:uuid_to_string(ClientId), Code, Description]),
              false
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem deleting sub categories from category ~s for user ~s: ~B - ~s", [Category, uuid:uuid_to_string(ClientId), Code, Description]),
          false
      end;
    _ ->
      system_error
  end.

delete_sub_category(ClientId, Category, SubCategory) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM sub_category WHERE user_id = ? AND category_name = ? AND name = ?">>,
        values = [{user_id, ClientId}, {category_name, Category}, {name, SubCategory}]}),
      case Result of
        {ok, void} ->
          true;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem deleting sub category ~s from category ~s for user ~s: ~B - ~s", [SubCategory, Category, uuid:uuid_to_string(ClientId), Code, Description]),
          false
      end;
    _ ->
      system_error
  end.

add_transaction(AccountId, Description, Category, SubCategory, Amount, Timestamp, Tags) ->
  AccountIdUUID = uuid:string_to_uuid(AccountId),
  TransactionId = uuid:get_v4(strong),
  case cqerl:get_client({}) of
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
      case Result of
        {ok, void} ->
          AddTagsMap = fun(Tag) -> add_tag_to_transaction(TransactionId, Tag) end,
          Results = lists:map(AddTagsMap, Tags),
          ResultOk = fun(SingleResult) -> SingleResult == ok end,
          case lists:all(ResultOk, Results) of
            true ->
              {ok, uuid:uuid_to_string(TransactionId, binary_standard)};
            _ ->
              system_error
          end;
        {error, {Code, ErrorDescription, _}} ->
          lager:log(error, self(), "Failed to add transaction [~s] to account ~s: ~B - ~s", [Description, AccountId, Code, ErrorDescription]),
          system_error
      end;
    _ ->
      system_error
  end.

add_tag_to_transaction(TransactionId, Tag) when is_integer(Tag) ->
  add_tag_to_transaction(TransactionId, integer_to_list(Tag));

add_tag_to_transaction(TransactionId, Tag) when is_list(Tag) ->
  case cqerl:get_client({}) of
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
          case Result2 of
            {ok, void} ->
              ok;
            {error, {Code, Description, _}} ->
              lager:log(error, self(), "Failed to add transaction [~s] to tag [~s]: ~B - ~s", [TransactionId, Tag, Code, Description]),
              system_error
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Failed to add tag [~s] to transaction [~s]: ~B - ~s", [Tag, TransactionId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


verify_transaction(AccountId, TransactionId, Timestamp) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = "SELECT description FROM transactions WHERE account_id = ? AND date = ? AND transaction_id = ?",
        values = [{account_id, AccountId}, {date, Timestamp}, {transaction_id, TransactionId}]}),
      case Result of
        {ok, Data} ->
          Categories = cqerl:all_rows(Data),
          case Categories of
            [] ->
              false;
            [_] ->
              true
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Error occured when checking transaction ~s: ~B - ~s", [TransactionId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


delete_transaction(AccountId, TransactionId, Timestamp) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM transactions WHERE account_id = ? AND date = ? AND transaction_id = ?">>,
        values = [{account_id, AccountId}, {date, Timestamp}, {transaction_id, TransactionId}]}),
      case Result of
        {ok, void} ->
          delete_transaction_tags(TransactionId);
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Error occured when deleting transaction ~s for account ~s: ~B - ~s", [TransactionId, AccountId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

delete_transaction_tags(TransactionId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{statement = <<"SELECT tag FROM tags_by_transaction WHERE transaction_id = ?">>,
        values = [{transaction_id, TransactionId}]}),
      case Result of
        {ok, Data} ->
          Result2 = delete_transaction_for_tag(cqerl:all_rows(Data), TransactionId),
          case Result2 of
            ok ->
              Result3 = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM tags_by_transaction WHERE transaction_id = ?">>,
                values = [{transaction_id, TransactionId}]}),
              case Result3 of
                {ok, void} ->
                  ok;
                {error, {Code, Description, _}} ->
                  lager:log(error, self(), "Error occured when deleting tags for transaction ~s: ~B - ~s", [TransactionId, Code, Description]),
                  system_error
              end;
            Error ->
              Error
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Error occured when fetching tags for transaction ~s: ~B - ~s", [TransactionId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

delete_transaction_for_tag([], _) ->
  ok;

delete_transaction_for_tag([H | T], TransactionId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Tag = proplists:get_value(tag, H),
      Result = cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM transactions_by_tag WHERE tag = ? AND transaction_id = ?">>,
        values = [{tag, Tag}, {transaction_id, TransactionId}]}),
      case Result of
        {ok, void} ->
          delete_transaction_for_tag(T, TransactionId);
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Error occured when deleting tag ~s for transaction ~s: ~B - ~s", [Tag, TransactionId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


add_account(Name, Type, StartBalance, Currency, UserID) ->
  AccountID = uuid:get_v4(strong),
  case cqerl:get_client({}) of
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
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem adding account [~s] for user ~s: ~B - ~s", [Name, uuid:uuid_to_string(UserID), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

insert_user_by_account(UserId, AccountId) ->
  case cqerl:get_client({}) of
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
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem adding user ~s to account ~s: ~B - ~s", [uuid:uuid_to_string(UserId), AccountId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

insert_account_by_user(AccountId, UserId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      Result = cqerl:run_query(Client, #cql_query{
        statement = <<"INSERT INTO accounts_by_user (user_id,account_id) VALUES (?,?)">>,
        values = [{user_id, UserId}, {account_id, AccountId}]}),
      case Result of
        {ok, void} ->
          ok;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem adding account ~s to user ~s: ~B - ~s", [AccountId, uuid:uuid_to_string(UserId), Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


delete_account(UserId, AccountId) ->
  Result = delete_all_account_transactions(get_account_transactions(AccountId, 0, erlang:system_time(milli_seconds)), AccountId),
  case Result of
    ok ->
      case cqerl:get_client({}) of
        {ok, Client} ->
          case cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM accounts WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
            {ok, void} ->
              delete_account_indexes(UserId, AccountId);
            {error, {Code, Description, _}} ->
              lager:log(error, self(), "Problem deleting account ~s: ~B - ~s", [AccountId, Code, Description]),
              system_error
          end;
        _ ->
          system_error
      end;
    _ ->
      Result
  end.

delete_account_indexes(UserId, AccountId) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM user_by_account WHERE account_id = ?">>, values = [{account_id, AccountId}]}) of
        {ok, void} ->
          case cqerl:run_query(Client, #cql_query{statement = <<"DELETE FROM accounts_by_user WHERE user_id = ? AND account_id = ?">>, values = [{user_id, UserId}, {account_id, AccountId}]}) of
            {ok, void} ->
              ok;
            {error, {Code, Description, _}} ->
              lager:log(error, self(), "Problem deleting account by user index ~s: ~B - ~s", [AccountId, Code, Description]),
              system_error
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem deleting user by account index ~s: ~B - ~s", [AccountId, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.


delete_all_account_transactions(Transactions, _) when is_atom(Transactions) ->
  Transactions;

delete_all_account_transactions(Transactions, AccountId) when is_list(Transactions) ->
  DeleteSingleTransaction = fun(Transaction) ->
    TransactionId = proplists:get_value(transaction_id, Transaction),
    Datetime = proplists:get_value(date, Transaction),
    case delete_transaction(AccountId, TransactionId, Datetime) of
      ok ->
        1;
      _ ->
        0
    end
                            end,
  Size = length(Transactions),
  Success = lists:sum(lists:map(DeleteSingleTransaction, Transactions)),
  if
    Size == Success -> ok;
    true ->
      system_error
  end.



check_account_auth(ClientId, AccountId) ->
  try
    AccountIdUUID = uuid:string_to_uuid(AccountId),
    case cqerl:get_client({}) of
      {ok, Client} ->
        case cqerl:run_query(Client, #cql_query{statement = <<"SELECT user_id FROM user_by_account WHERE account_id = ?">>, values = [{account_id, AccountIdUUID}]}) of
          {ok, Result} ->
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
          {error, {Code, Description, _}} ->
            lager:log(error, self(), "Problem checking account access ~s to user ~s: ~B - ~s", [AccountId, uuid:uuid_to_string(ClientId), Code, Description]),
            system_error
        end;
      _ ->
        system_error
    end
  catch
    exit:badarg -> not_found
  end.

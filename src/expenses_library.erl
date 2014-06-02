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

%% API
-export([get_transactions/2, get_accounts/1, get_all_categories/0, get_all_subcategories/0, add_transaction/7, check_account_auth/2, get_all_tags/0]).


get_transactions(ClientId, AccountId) ->
  Con = datasource:get_connection(mysql_datasource),
  Statement1 = connection:get_prepared_statement_handle(Con, "SELECT accountName FROM Account WHERE accountId = ? AND userId = ?"),
  {_, Accounts} = connection:execute_statement(Con, Statement1, [8, 8], [AccountId, ClientId]),
  case Accounts of
    [] ->
      datasource:return_connection(mysql_datasource, Con),
      not_found;
    [_] ->
      Statement2 = connection:get_prepared_statement_handle(Con, "SELECT T.transactionId,T.description,T.accountId,T.categoryId,T.subCategoryId,T.date,T.amount,T.externalReference FROM Account A,Transactions T WHERE A.accountId=T.accountId AND A.userId = ? AND T.accountId = ?"),
      {_, Rows} = connection:execute_statement(Con, Statement2, [8, 8], [ClientId, AccountId]),
      datasource:return_connection(mysql_datasource, Con),
      Rows;
    _ ->
      datasource:return_connection(mysql_datasource, Con),
      system_error
  end.


get_accounts(ClientId) ->
  Con = datasource:get_connection(mysql_datasource),
  Statement1 = connection:get_prepared_statement_handle(Con, "SELECT accountId, accountName, accountType, startBalance, currency FROM Account WHERE userId = ?"),
  {Meta, Accounts} = connection:execute_statement(Con, Statement1, [8], [ClientId]),
  case Accounts of
    [] ->
      datasource:return_connection(mysql_datasource, Con),
      not_found;
    [_ | _] ->
      Statement2 = connection:get_prepared_statement_handle(Con, "SELECT accountId,SUM(amount) FROM Transactions WHERE accountId IN (SELECT accountId FROM Account WHERE userId = ?) GROUP BY accountId;"),
      {_, ProcessedBalances} = connection:execute_statement(Con, Statement2, [8], [ClientId]),
      Map1 = fun([Key, Value]) -> {Key, Value} end,
      ProcessedBalances1 = lists:map(Map1, ProcessedBalances),
      Map2 = fun([AcctId, AcctName, AcctType, StartBalance, Currency]) ->
        Value = proplists:get_value(AcctId, ProcessedBalances1),
        case Value of
          undefined ->
            {[{<<"acct">>, AcctId}, {<<"name">>, AcctName}, {<<"type">>, AcctType}, {<<"startBal">>, StartBalance}, {<<"cur">>, Currency}, {<<"bal">>, 0}]};
          Balance ->
            {[{<<"acct">>, AcctId}, {<<"name">>, AcctName}, {<<"type">>, AcctType}, {<<"startBal">>, StartBalance}, {<<"cur">>, Currency}, {<<"bal">>, Balance}]}

        end
      end,
      datasource:return_connection(mysql_datasource, Con),
      lists:map(Map2, Accounts);
    _ ->
      datasource:return_connection(mysql_datasource, Con),
      system_error
  end.


get_all_categories() ->
  Con = datasource:get_connection(mysql_datasource),
  {_, Categories} = connection:execute_query(Con, "SELECT categoryId,description FROM Category"),
  datasource:return_connection(mysql_datasource, Con),
  Categories.


get_all_subcategories() ->
  Con = datasource:get_connection(mysql_datasource),
  {_, SubCategories} = connection:execute_query(Con, "SELECT subCategoryId,description,categoryId FROM SubCategory"),
  datasource:return_connection(mysql_datasource, Con),
  SubCategories.


add_transaction(AccountId, Description, CategoryId, SubCategoryId, Amount, Timestamp, Tags) ->
  Con = datasource:get_connection(mysql_datasource),
  Statement = connection:get_prepared_statement_handle(Con, "INSERT INTO Transactions (description,accountId,categoryId,subCategoryId,date,amount) VALUES(?,?,?,?,?,?);"),
  Result = connection:execute_statement(Con, Statement, [253, 8, 8, 8, 8, 5], [Description, AccountId, CategoryId, SubCategoryId, Timestamp, Amount]),
  case Result of
    {_, [{ok_packet, _, NewId, _, _, _}]} ->
      Statement2 = connection:get_prepared_statement_handle(Con, "INSERT INTO TransactionTag (transactionId,tagId) VALUES(?,?)"),
      Result2 = assign_tag_to_transaction(Tags, NewId, Statement2, Con),
      datasource:return_connection(mysql_datasource, Con),
      Result2;
    _ ->
      datasource:return_connection(mysql_datasource, Con),
      system_error
  end.

assign_tag_to_transaction([H | T], NewId, Statement, Con) ->
  case connection:execute_statement(Con, Statement, [8, 8], [NewId, H]) of
    {_, [{ok_packet, _, _, _, _, _}]} ->
      assign_tag_to_transaction(T, NewId, Statement, Con);
    _ ->
      system_error
  end;

assign_tag_to_transaction([], _, _, _) ->
  ok.


check_account_auth(Client, Account) ->
  Con = datasource:get_connection(mysql_datasource),
  Statement1 = connection:get_prepared_statement_handle(Con, "SELECT accountName FROM Account WHERE accountId = ? AND userId = ?"),
  {_, Accounts} = connection:execute_statement(Con, Statement1, [8, 8], [Account, Client]),
  datasource:return_connection(mysql_datasource, Con),
  case Accounts of
    [] ->
      denied;
    [_] ->
      valid;
    _ ->
      system_error
  end.

get_all_tags() ->
  Con = datasource:get_connection(mysql_datasource),
  {_, Tags} = connection:execute_query(Con, "SELECT tagId,description FROM Tag"),
  datasource:return_connection(mysql_datasource, Con),
  Tags.









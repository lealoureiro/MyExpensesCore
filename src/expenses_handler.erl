
%% @doc Expenses Gateway Root Handler.
-module(expenses_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = maybe_reply(Method, HasBody, Req2),
  {ok, Req3, State}.


maybe_reply(<<"POST">>, _, Req) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  {Path, Req3} = cowboy_req:path(Req2),
  AccessToken = proplists:get_value(<<"token">>, PostVals),
  io:format("Requested: ~s Token: ~s ~n", [Path, AccessToken]),
  case auth_library:auth(AccessToken) of
    {ok, ClientId} ->
      process(Path, ClientId, PostVals, Req3);
    {not_valid} ->
      cowboy_req:reply(403, Req);
    _ ->
      cowboy_req:reply(500, Req)
  end;

maybe_reply(_, _, Req) ->
  cowboy_req:reply(405, Req).


process(<<"/expenses/echo">>, _, PostVals, Req) ->
  Echo = proplists:get_value(<<"echo">>, PostVals),
  case Echo of
    undefined ->
      missing_parameter(Req);
    _ ->
      Data = {[{<<"echo">>, Echo}]},
      reply(Data, Req)
  end;

process(<<"/expenses/get_transactions">>, ClientId, PostVals, Req) ->
  Acct = proplists:get_value(<<"acct">>, PostVals),
  io:format("Processing transactions, Client ~s Acccount: ~s ~n", [uuid:uuid_to_string(ClientId), Acct]),
  case Acct of
    undefined ->
      missing_parameter(Req);
    _ ->
      Transactions = expenses_library:get_transactions(ClientId, Acct),
      case Transactions of
        not_found ->
          cowboy_req:reply(403, Req);
        access_denied ->
          cowboy_req:reply(403, Req);
        [_ | _] ->
          Mapping = fun(Transaction) ->
            Id = list_to_binary(uuid:uuid_to_string(proplists:get_value(transaction_id, Transaction))),
            Description = proplists:get_value(description, Transaction),
            CtgID = proplists:get_value(category, Transaction),
            SubCtgId = proplists:get_value(sub_category, Transaction),
            Datetime = proplists:get_value(date, Transaction),
            Amt = proplists:get_value(amount, Transaction),
            ExtRef = proplists:get_value(external_reference, Transaction),
            case ExtRef of
              undefined ->
                {[{<<"id">>, Id}, {<<"desc">>, Description}, {<<"ctgId">>, CtgID}, {<<"subCtgId">>, SubCtgId}, {<<"datetime">>, Datetime}, {<<"amt">>, Amt}, {<<"extRef">>, null}]};
              _ ->
                {[{<<"id">>, Id}, {<<"desc">>, Description}, {<<"ctgId">>, CtgID}, {<<"subCtgId">>, SubCtgId}, {<<"datetime">>, Datetime}, {<<"amt">>, Amt}, {<<"extRef">>, ExtRef}]}
            end
          end,
          Data = lists:map(Mapping, Transactions),
          reply(Data, Req);
        [] ->
          reply([], Req);
        _ ->
          cowboy_req:reply(500, Req)
      end
  end;

process(<<"/expenses/get_accounts">>, ClientId, _, Req) ->
  Accounts = expenses_library:get_accounts(ClientId),
  case Accounts of
    not_found ->
      reply([], Req);
    [_ | _] ->
      Map = fun(Account) ->
        AccountId = list_to_binary(uuid:uuid_to_string(proplists:get_value(account_id, Account))),
        AccountName = proplists:get_value(name, Account),
        AcctType = proplists:get_value(account_type, Account),
        StartBalance = proplists:get_value(start_balance, Account),
        Currency = proplists:get_value(currency, Account),
        Balance = proplists:get_value(balance, Account),
        {[{<<"acct">>, AccountId}, {<<"name">>, AccountName}, {<<"type">>, AcctType}, {<<"startBal">>, StartBalance}, {<<"cur">>, Currency}, {<<"bal">>, Balance}]}
      end,
      Data = lists:map(Map, Accounts),
      reply(Data, Req);
    [] ->
      reply([], Req);
    _ ->
      cowboy_req:reply(500, Req)
  end;


process(<<"/expenses/get_categories">>, _, _, Req) ->
  Categories = expenses_library:get_all_categories(),
  SubCategories = expenses_library:get_all_subcategories(),
  case {Categories, SubCategories} of
    {system_error, _} ->
      cowboy_req:reply(500, Req);
    {_, system_error} ->
      cowboy_req:reply(500, Req);
    _ ->
      Map1 = fun(Category) ->
        CategoryName = proplists:get_value(name, Category),
        {[{<<"name">>, CategoryName}]}
      end,
      Map2 = fun(SubCategory) ->
        CategoryName = proplists:get_value(category_name, SubCategory),
        Name = proplists:get_value(name, SubCategory),
        {[{<<"cat">>, CategoryName}, {<<"subCat">>, Name}]}
      end,
      Categories1 = lists:map(Map1, Categories),
      SubCategories1 = lists:map(Map2, SubCategories),
      Data = {[{<<"Categories">>, Categories1}, {<<"SubCategories">>, SubCategories1}]},
      reply(Data, Req)
  end;


process(<<"/expenses/add_transaction">>, ClientId, PostVals, Req) ->
  Account = proplists:get_value(<<"acct">>, PostVals),
  case Account of
    undefined ->
      missing_parameter(Req);
    Account ->
      case expenses_library:check_account_auth(ClientId, Account) of
        valid ->
          add_transaction(Account, PostVals, Req);
        denied ->
          cowboy_req:reply(403, Req);
        not_found ->
          cowboy_req:reply(403, Req);
        _ ->
          cowboy_req:reply(500, Req)
      end
  end;

process(_, _, _, Req) ->
  cowboy_req:reply(405, Req).


add_transaction(Account, PostVals, Req) ->
  Valid = proplists:is_defined(<<"dsc">>, PostVals)
    and proplists:is_defined(<<"cat">>, PostVals)
    and proplists:is_defined(<<"subCat">>, PostVals)
    and proplists:is_defined(<<"amt">>, PostVals)
    and proplists:is_defined(<<"timestamp">>, PostVals)
    and proplists:is_defined(<<"tags">>, PostVals),
  case Valid of
    false ->
      missing_parameter(Req);
    true ->
      Description = proplists:get_value(<<"dsc">>, PostVals),
      Category = binary_to_list(proplists:get_value(<<"cat">>, PostVals)),
      SubCategory = binary_to_list(proplists:get_value(<<"subCat">>, PostVals)),
      {Amount, _} = string:to_float(binary_to_list(proplists:get_value(<<"amt">>, PostVals))),
      {Timestamp, _} = string:to_integer(binary_to_list(proplists:get_value(<<"timestamp">>, PostVals))),
      Tags = proplists:get_value(<<"tags">>, PostVals),
      TagsList = string:tokens(binary_to_list(Tags), ","),
      Result = expenses_library:add_transaction(Account, Description, Category, SubCategory, Amount, Timestamp, TagsList),
      case Result of
        ok ->
          reply([], Req);
        system_error ->
          cowboy_req:reply(400, Req)
      end
  end.


reply(Data, Req) ->
  EchoJSON = jiffy:encode(Data, [force_utf8]),
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>}
  ], EchoJSON, Req).

missing_parameter(Req) ->
  cowboy_req:reply(400, [], <<"Missing a parameter!">>, Req).

terminate(_Reason, _Req, _State) ->
  ok.




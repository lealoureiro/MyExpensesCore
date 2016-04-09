%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2016 11:18 PM
%%%-------------------------------------------------------------------
-module(accounts_handler).
-author("leandro").

%% API
-export([init/3]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_json/2]).
-export([process_post/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, process_post}], Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      {{false, <<"Key">>}, Req, State};
    {Token, _} ->
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          case cowboy_req:binding(id, Req) of
            {undefined, _} ->
              Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
              {true, Req2, State};
            {AccountId, _} ->
              case expenses_library:check_account_auth(ClientId, AccountId) of
                valid ->
                  lager:log(info, self(), "Client ~s requested account ~s ~n", [uuid:uuid_to_string(ClientId), AccountId]),
                  Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
                  Req3 = cowboy_req:set_meta(<<"accountId">>, AccountId, Req2),
                  {true, Req3, State};
                _ ->
                  {{false, <<"Key">>}, Req, State}
              end
          end;
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.



get_json(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  lager:log(info, self(), "Client ~s requested accounts ~n", [uuid:uuid_to_string(ClientId)]),
  case cowboy_req:meta(<<"accountId">>, Req) of
    {undefined, _} ->
      get_all_accounts(ClientId, Req, State);
    {AccountId, _} ->
      get_account_detail_info(AccountId, Req, State)
  end.


get_all_accounts(ClientId, Req, State) ->
  Accounts = expenses_library:get_client_accounts(ClientId),
  case Accounts of
    not_found ->
      {<<"[]">>, Req, State};
    [_ | _] ->
      Map = fun(Account) ->
        AccountId = list_to_binary(uuid:uuid_to_string(proplists:get_value(account_id, Account))),
        AccountName = proplists:get_value(name, Account),
        AcctType = proplists:get_value(account_type, Account),
        StartBalance = proplists:get_value(start_balance, Account),
        Currency = proplists:get_value(currency, Account),
        {[{<<"id">>, AccountId}, {<<"name">>, AccountName}, {<<"type">>, AcctType}, {<<"startBalance">>, StartBalance}, {<<"currency">>, Currency}]}
            end,
      Data = lists:map(Map, Accounts),
      JSON = jiffy:encode(Data),
      {JSON, Req, State};
    [] ->
      {<<"[]">>, Req, State};
    _ ->
      {false, Req, State}
  end.

get_account_detail_info(AccountId, Req, State) ->
  lager:log(info, self(), "Fetching information for account ~s ~n", [AccountId]),
  case expenses_library:get_account_info(AccountId) of
    system_error ->
      cowboy_req:reply(500, [{<<"connection">>, <<"close">>}], Req),
      {halt, Req, State};
    Account ->
      case expenses_library:get_account_transactions_balance(AccountId) of
        system_error ->
          cowboy_req:reply(500, [{<<"connection">>, <<"close">>}], Req),
          {halt, Req, State};
        AccountBalance ->
          AccountIdString = list_to_binary(uuid:uuid_to_string(proplists:get_value(account_id, Account))),
          AccountName = proplists:get_value(name, Account),
          AcctType = proplists:get_value(account_type, Account),
          StartBalance = proplists:get_value(start_balance, Account),
          Currency = proplists:get_value(currency, Account),
          Balance = proplists:get_value(balance, AccountBalance),
          Transactions = proplists:get_value(transactions, AccountBalance),
          Data = {[{<<"id">>, AccountIdString}, {<<"name">>, AccountName}, {<<"type">>, AcctType}, {<<"balance">>, Balance}, {<<"transactions">>, Transactions}, {<<"startBalance">>, StartBalance}, {<<"currency">>, Currency}]},
          JSON = jiffy:encode(Data),
          {JSON, Req, State}
      end
  end.

process_post(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  lager:log(info, self(), "Client ~s adding new account~n", [uuid:uuid_to_string(ClientId)]),
  {ok, Body, _} = cowboy_req:body(Req),
  try
    {Data} = jiffy:decode(Body),
    Valid = proplists:is_defined(<<"name">>, Data) and proplists:is_defined(<<"type">>, Data) and proplists:is_defined(<<"startBalance">>, Data) and proplists:is_defined(<<"currency">>, Data),
    case Valid of
      true ->
        Name = proplists:get_value(<<"name">>, Data),
        Type = proplists:get_value(<<"type">>, Data),
        StartBalance = proplists:get_value(<<"startBalance">>, Data),
        Currency = proplists:get_value(<<"currency">>, Data),
        Result = expenses_library:add_account(Name, Type, StartBalance, Currency, ClientId),
        case Result of
          {ok, AccountId} ->
            Output = {[{<<"id">>, AccountId}]},
            JSON = jiffy:encode(Output),
            Resp = cowboy_req:set_resp_body(JSON, Req),
            {true, Resp, State};
          system_error ->
            lager:log(info, self(), "Client ~s problem when creating account~n", [uuid:uuid_to_string(ClientId)]),
            {false, Req, State}
        end;
      false ->
        lager:log(info, self(), "Client ~s missing parameter for new account~n", [uuid:uuid_to_string(ClientId)]),
        {false, Req, State}
    end
  catch
    throw:{error, _} ->
      lager:log(info, self(), "Client ~s sending bad request for new account~n", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State};
    error:_ ->
      lager:log(info, self(), "Client ~s sending invalid data for new account~n", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.


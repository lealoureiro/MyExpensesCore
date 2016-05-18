%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2016 11:18 PM
%%%-------------------------------------------------------------------
-module(account_detail_handler).
-author("leandro").

%% API
-export([init/3]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_json/2]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.


allowed_methods(Req, State) ->
  {[<<"GET">>, <<"OPTIONS">>], Req, State}.


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
          {AccountId, _} = cowboy_req:binding(id, Req),
          case expenses_library:check_account_auth(ClientId, AccountId) of
            valid ->
              lager:log(info, self(), "Client ~s requested account ~s ~n", [uuid:uuid_to_string(ClientId), AccountId]),
              Req2 = cowboy_req:set_meta(<<"accountId">>, AccountId, Req),
              {true, Req2, State};
            _ ->
              {{false, <<"Key">>}, Req, State}
          end;
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.


get_json(Req, State) ->
  {AccountId, _} = cowboy_req:meta(<<"accountId">>, Req),
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

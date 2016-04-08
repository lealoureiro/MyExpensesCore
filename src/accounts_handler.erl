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
-export([get_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

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
              lager:log(info, self(), "Client ~s requested accounts ~n", [uuid:uuid_to_string(ClientId)]),
              Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
              {true, Req2, State};
            {AccountId, _} ->
              case expenses_library:check_account_auth(ClientId, AccountId) of
                valid ->
                  lager:log(info, self(), "Client ~s requested account ~s ~n", [uuid:uuid_to_string(ClientId), AccountId]),
                  Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
                  Req3 = cowboy_req:set_meta(<<"accountId">>, ClientId, Req2),
                  {true, Req3, State};
                _ ->
                  {{false, <<"Key">>}, Req, State}
              end
          end;
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

get_json(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
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
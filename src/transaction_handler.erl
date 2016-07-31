%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2016 2:31 PM
%%%-------------------------------------------------------------------
-module(transaction_handler).
-author("leandro").

%% API
-export([init/3,
  allowed_methods/2,
  is_authorized/2,
  resource_exists/2,
  delete_resource/2]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      {{false, <<"Key">>}, Req, State};
    {Token, _} ->
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          {AccountId, _} = cowboy_req:binding(accountId, Req),
          case expenses_library:check_account_auth(ClientId, AccountId) of
            valid ->
              Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
              Req3 = cowboy_req:set_meta(<<"accountId">>, AccountId, Req2),
              {TransactionId, _} = cowboy_req:binding(transactionId, Req),
              Req4 = cowboy_req:set_meta(<<"transactionId">>, TransactionId, Req3),
              {true, Req4, State};
            _ ->
              {{false, <<"Key">>}, Req, State}
          end;
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

resource_exists(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {AccountId, _} = cowboy_req:meta(<<"accountId">>, Req),
  {TransactionId, _} = cowboy_req:meta(<<"transactionId">>, Req),
  lager:log(info, self(), "Client ~s accessing transaction ~s for account ~s", [uuid:uuid_to_string(ClientId), AccountId, TransactionId]),
  {ok, Body, _} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  Timestamp = proplists:get_value(<<"timestamp">>, Data),
  Result = expenses_library:verify_transaction(AccountId, TransactionId, Timestamp),
  {Result, Req, State}.

delete_resource(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {AccountId, _} = cowboy_req:meta(<<"accountId">>, Req),
  {TransactionId, _} = cowboy_req:meta(<<"transactionId">>, Req),
  lager:log(info, self(), "Client ~s deleting transaction ~s", [uuid:uuid_to_string(ClientId), TransactionId]),
  {ok, Body, _} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  Timestamp = proplists:get_value(<<"timestamp">>, Data),
  case expenses_library:delete_transaction(AccountId, TransactionId, Timestamp) of
    ok ->
      {true, Req, State};
    _ ->
      {false, Req, State}
  end.
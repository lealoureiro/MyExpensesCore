%%%-------------------------------------------------------------------
%%% @author leandro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2016 10:16 PM
%%%-------------------------------------------------------------------
-module(transactions_handler).
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

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      {{false, <<"Key">>}, Req, State};
    {Token, _} ->
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          {AccountId, _Req} = cowboy_req:binding(accountId, Req),
          case expenses_library:check_account_auth(ClientId, AccountId) of
            valid ->
              Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
              Req3 = cowboy_req:set_meta(<<"accountId">>, AccountId, Req2),
              {true, Req3, State};
            _ ->
              {{false, <<"Key">>}, Req, State}
          end;
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

get_json(Req, State) ->
  {ClientId, _Req2} = cowboy_req:meta(<<"clientId">>, Req),
  {AccountId, _Req3} = cowboy_req:meta(<<"accountId">>, Req),
  lager:log(info, self(), "Client ~s requested transactions for account ~s~n", [uuid:uuid_to_string(ClientId), AccountId]),
  get_account_transactions(AccountId, Req, State).

get_account_transactions(AccountId, Req, State) ->
  Transactions = expenses_library:get_account_transactions(AccountId),
  case Transactions of
    not_found ->
      cowboy_req:reply(404, [{<<"connection">>, <<"close">>}], Req),
      {halt, Req, State};
    system_error ->
      cowboy_req:reply(500, [{<<"connection">>, <<"close">>}], Req),
      {halt, Req, State};
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
            {[{<<"id">>, Id}, {<<"description">>, Description}, {<<"categoryId">>, CtgID}, {<<"subCategoryId">>, SubCtgId}, {<<"datetime">>, Datetime}, {<<"amount">>, Amt}, {<<"externalReference">>, null}]};
          _ ->
            {[{<<"id">>, Id}, {<<"description">>, Description}, {<<"categoryId">>, CtgID}, {<<"subCategoryId">>, SubCtgId}, {<<"datetime">>, Datetime}, {<<"amount">>, Amt}, {<<"externalReference">>, ExtRef}]}
        end
                end,
      Data = lists:map(Mapping, Transactions),
      JSON = jiffy:encode(Data),
      {JSON, Req, State};
    [] ->
      JSON = jiffy:encode([]),
      {JSON, Req, State}
  end.


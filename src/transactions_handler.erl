%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2016 10:16 PM
%%%-------------------------------------------------------------------
-module(transactions_handler).
-author("leandro").

%% API
-export([init/3,
  allowed_methods/2,
  is_authorized/2,
  content_types_provided/2,
  content_types_accepted/2,
  get_json/2,
  process_post/2]).

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
  lager:log(info, self(), "Client ~s requested transactions for account ~s", [uuid:uuid_to_string(ClientId), AccountId]),
  get_account_transactions(AccountId, Req, State).

get_account_transactions(AccountId, Req, State) ->
  {Start, _} = cowboy_req:qs_val(<<"start">>, Req, 0),
  {End, _} = cowboy_req:qs_val(<<"end">>, Req, erlang:system_time(milli_seconds)),
  Transactions = expenses_library:get_account_transactions(AccountId, Start, End),
  case Transactions of
    not_found ->
      cowboy_req:reply(404, Req),
      {halt, Req, State};
    system_error ->
      lager:log(error, self(), "Failed to get transactions for account ~s", [AccountId]),
      cowboy_req:reply(500, Req),
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
            {[{<<"id">>, Id}, {<<"description">>, Description}, {<<"category">>, CtgID}, {<<"subCategory">>, SubCtgId}, {<<"timestamp">>, Datetime}, {<<"amount">>, Amt}, {<<"externalReference">>, null}]};
          _ ->
            {[{<<"id">>, Id}, {<<"description">>, Description}, {<<"category">>, CtgID}, {<<"subCategory">>, SubCtgId}, {<<"timestamp">>, Datetime}, {<<"amount">>, Amt}, {<<"externalReference">>, ExtRef}]}
        end
                end,
      Data = lists:map(Mapping, Transactions),
      JSON = jiffy:encode(Data),
      {JSON, Req, State};
    [] ->
      JSON = jiffy:encode([]),
      {JSON, Req, State}
  end.

process_post(Req, State) ->
  {ClientId, _Req2} = cowboy_req:meta(<<"clientId">>, Req),
  {AccountId, _Req1} = cowboy_req:meta(<<"accountId">>, Req),
  lager:log(info, self(), "Client ~s adding new transaction to account ~s~n", [uuid:uuid_to_string(ClientId), AccountId]),
  {ok, Body, _} = cowboy_req:body(Req),
  try
    {Data} = jiffy:decode(Body),
    Valid = proplists:is_defined(<<"description">>, Data) and proplists:is_defined(<<"category">>, Data) and proplists:is_defined(<<"subCategory">>, Data) and proplists:is_defined(<<"amount">>, Data) and proplists:is_defined(<<"timestamp">>, Data) and proplists:is_defined(<<"tags">>, Data),
    case Valid of
      true ->
        Description = proplists:get_value(<<"description">>, Data),
        Category = binary_to_list(proplists:get_value(<<"category">>, Data)),
        SubCategory = binary_to_list(proplists:get_value(<<"subCategory">>, Data)),
        Amount = proplists:get_value(<<"amount">>, Data),
        Timestamp = proplists:get_value(<<"timestamp">>, Data),
        Tags = proplists:get_value(<<"tags">>, Data),
        Mapping = fun(Tag) -> binary_to_list(Tag) end,
        Tags2 = lists:map(Mapping, Tags),
        Result = expenses_library:add_transaction(AccountId, Description, Category, SubCategory, Amount, Timestamp, Tags2),
        case Result of
          {ok, TransactionId} ->
            Output = {[{<<"id">>, TransactionId}]},
            JSON = jiffy:encode(Output),
            Resp = cowboy_req:set_resp_body(JSON, Req),
            {true, Resp, State};
          system_error ->
            lager:log(info, self(), "Client ~s problem when adding transaction~n", [uuid:uuid_to_string(ClientId)]),
            {false, Req, State}
        end;
      false ->
        lager:log(info, self(), "Client ~s missing parameter for new transaction~n", [uuid:uuid_to_string(ClientId)]),
        {false, Req, State}
    end
  catch
    throw:{error, _} ->
      lager:log(info, self(), "Client ~s sending bad request for new transaction~n", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.
%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 26. Jun 2016 4:57 PM
%%%-------------------------------------------------------------------
-module(sub_categories_handler).
-author("leandro").

%% API
-export([init/3,
  allowed_methods/2,
  allow_missing_post/2,
  is_authorized/2,
  resource_exists/2,
  content_types_accepted/2,
  process_post/2]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

allow_missing_post(Req, State) ->
  {false, Req, State}.

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
          Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
          {Category, _} = cowboy_req:binding(categoryName, Req),
          Req3 = cowboy_req:set_meta(<<"category">>, Category, Req2),
          {true, Req3, State};
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

resource_exists(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {Category, _} = cowboy_req:meta(<<"category">>, Req),
  lager:log(info, self(), "Client ~s accessing category ~s", [uuid:uuid_to_string(ClientId), Category]),
  Result = expenses_library:verify_category(ClientId, Category),
  {Result, Req, State}.

process_post(Req, State) ->
  {ClientId, _Req2} = cowboy_req:meta(<<"clientId">>, Req),
  {Category, _Req1} = cowboy_req:meta(<<"category">>, Req),
  {ok, Body, _} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  Valid = proplists:is_defined(<<"name">>, Data),
  case Valid of
    true ->
      Name = proplists:get_value(<<"name">>, Data),
      lager:log(info, self(), "Client ~s adding new sub category ~s to category ~s", [uuid:uuid_to_string(ClientId), Name, Category]),
      case expenses_library:add_sub_category(ClientId, Category, Name) of
        true ->
          {true, Req, State};
        _ ->
          {false, Req, State}
      end;
    _ ->
      lager:log(info, self(), "Client ~s missing parameter for new sub category~n", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.
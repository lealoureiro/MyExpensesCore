%%%-------------------------------------------------------------------
%%% @author leandro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2016 8:04 PM
%%%-------------------------------------------------------------------
-module(tags_handler).
-author("leandro").

%% API
-export([
  init/3,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  is_authorized/2,
  get_json/2,
  process_put/2
]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"PUT">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, process_put}], Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      {{false, <<"Key">>}, Req, State};
    {Token, _} ->
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
          {true, Req2, State};
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

get_json(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  lager:log(info, self(), "Client ~s requested tags ~n", [uuid:uuid_to_string(ClientId)]),
  Tags = expenses_library:get_tags(ClientId),
  case Tags of
    not_found ->
      {<<"[]">>, Req, State};
    [_ | _] ->
      Map = fun(Tag) ->
        TagName = proplists:get_value(tag, Tag),
        Default = proplists:get_value(default, Tag),
        {[{<<"name">>, TagName}, {<<"defaultSelected">>, Default}]}
            end,
      Data = lists:map(Map, Tags),
      JSON = jiffy:encode(Data),
      {JSON, Req, State};
    [] ->
      {<<"[]">>, Req, State};
    _ ->
      lager:log(error, self(), "Failed to fetch tags for user ~s", [uuid:uuid_to_string(ClientId)]),
      cowboy_req:reply(500, Req),
      {halt, Req, State}
  end.

process_put(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {ok, Body, _} = cowboy_req:body(Req),
  {Data} = jiffy:decode(Body),
  Valid = proplists:is_defined(<<"name">>, Data) and proplists:is_defined(<<"defaultSelected">>, Data),
  case Valid of
    true ->
      TagName = proplists:get_value(<<"name">>, Data),
      Default = proplists:get_value(<<"defaultSelected">>, Data),
      lager:log(info, self(), "Client ~s adding new tag ~s~n", [uuid:uuid_to_string(ClientId), TagName]),
      case expenses_library:add_tag(ClientId, TagName, Default) of
        ok ->
          {true, Req, State};
        _ ->
          {false, Req, State}
      end;
    _ ->
      lager:log(info, self(), "Client ~s missing parameter for new tag~n", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.
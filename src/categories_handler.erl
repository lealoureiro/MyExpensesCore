%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2016 4:50 PM
%%%-------------------------------------------------------------------
-module(categories_handler).
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
          Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
          {true, Req2, State};
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

get_json(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  lager:log(info, self(), "Client ~s requested gategories ~n", [uuid:uuid_to_string(ClientId)]),
  Categories = expenses_library:get_all_categories(ClientId),
  case Categories of
    [] ->
      {<<"[]">>, Req, State};
    [_ | _] ->
      Map = fun(Category) -> proplists:get_value(name, Category) end,
      Data = lists:map(Map, Categories),
      JSON = jiffy:encode(Data),
      {JSON, Req, State};
    _ ->
      lager:log(error, self(), "A problem occurred when fetching categories for user ~s", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.



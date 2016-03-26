%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2016 21:21
%%%-------------------------------------------------------------------
-module(sessions_handler).
-author("leandroloureiro").

%% API
-export([init/3]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

get_json(Req, State) ->
  lager:log(info, self(), "Requested check authentication ~n"),
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      access_denied(Req);
    {_, _} ->
      {<<"">>, Req, State}
  end.

access_denied(Req) ->
  cowboy_req:reply(403, [{<<"connection">>, <<"close">>}], Req).
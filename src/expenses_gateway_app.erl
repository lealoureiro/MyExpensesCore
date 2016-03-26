
-module(expenses_gateway_app).
-behaviour(application).

-include("sessions_records.hrl").

-export([start/2]).
-export([stop/1]).



start(_Type, _Args) ->
  lager:log(info, self(), "Initializing MNESIA database... ~n"),
  startDatabase(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/expenses/[...]", expenses_handler, []},
      {"/auth/[...]", auth_handler, []},
      {"/sessions", sessions_handler, []}
    ]}
  ]),
  lager:log(info, self(), "Starting HTTP Server... ~n"),
  {ok, _} = cowboy:start_http(http, 100, [
    {port, 8081}
  ],
    [
      {env, [{dispatch, Dispatch}]},
      {onresponse, fun custom_on_response/4}
    ]),
  lager:log(info, self(), "Starting CORE Application... ~n"),
  expenses_gateway_sup:start_link().

stop(_State) ->
  ok.


startDatabase() ->
  mnesia:start(),
  mnesia:create_table(sessions, [{attributes, record_info(fields, sessions)}]).


custom_on_response(StatusCode, Headers, Body, Req) ->
  Headers2 = lists:keyreplace(<<"server">>, 1, Headers, {<<"server">>, <<"MyExpensesCore Server v0.1.0">>}),
  {ok, Req2} = cowboy_req:reply(StatusCode, Headers2, Body, Req),
  Req2.



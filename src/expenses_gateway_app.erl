%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(expenses_gateway_app).
-behaviour(application).

-include("deps/mysql_client/include/client_records.hrl").
-include("include/sessions_records.hrl").

%% API.
-export([start/2]).
-export([stop/1]).

%% API.


start(_Type, _Args) ->
  io:format("Initializing MNESIA database... ~n"),
  startDatabase(),
  io:format("Connection MySQL database... ~n"),
  my:new_datasource(mysql_datasource, #datasource{host = "localhost", port = 3306, database = "MFINANCES_DEV", user = "mfinances", password = "mfinances"}),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/expenses/[...]", expenses_handler, []},
      {"/auth/[...]", auth_handler, []}
    ]}
  ]),
  io:format("Starting HTTP Server... ~n"),
  {ok, _} = cowboy:start_http(http, 100, [
    {port, 8081}
  ],
    [
      {env, [{dispatch, Dispatch}]},
      {onresponse, fun custom_onresponse/4}
    ]),
  io:format("Starting CORE Application... ~n"),
  expenses_gateway_sup:start_link().

stop(_State) ->
  ok.


startDatabase() ->
  mnesia:start(),
  mnesia:create_table(sessions, [{attributes, record_info(fields, sessions)}]).


custom_onresponse(StatusCode, Headers, Body, Req) ->
  Headers2 = lists:keyreplace(<<"server">>, 1, Headers, {<<"server">>, <<"Expenses Gateway Server v0.1">>}),
  {ok, Req2} = cowboy_req:reply(StatusCode, Headers2, Body, Req),
  Req2.



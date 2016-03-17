%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2014 22:36
%%%-------------------------------------------------------------------
-module(auth_handler).
-author("leandroloureiro").

%% API
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).



init(_Transport, Req, []) ->
  {ok, Req, undefined}.


handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = maybe_reply(Method, HasBody, Req2),
  {ok, Req3, State}.


maybe_reply(<<"POST">>, _, Req) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  {Path, Req3} = cowboy_req:path(Req2),
  lager:log(info, self(), "Requested: ~s ~n", [Path]),
  process(Path, PostVals, Req3);

maybe_reply(_, _, Req) ->
  method_not_allowed(Req).


process(<<"/auth/login">>, PostVals, Req) ->
  Username = proplists:get_value(<<"username">>, PostVals),
  Password = proplists:get_value(<<"password">>, PostVals),
  login(Username, Password, Req);

process(<<"/auth/logout">>, PostVals, Req) ->
  Token = binary_to_list(proplists:get_value(<<"token">>, PostVals)),
  logout(Token, Req).


login(undefined, _, Req) ->
  missing_parameter(Req);

login(_, undefined, Req) ->
  missing_parameter(Req);

login(Username, Password, Req) ->
  case auth_library:login(Username, Password) of
    not_found ->
      access_denied(Req);
    {Token, Id, Name} ->
      Data = {[{<<"token">>, Token}, {<<"id">>, Id}, {<<"name">>, Name}]},
      reply(Data, Req);
    _ ->
      server_error(Req)
  end.


logout(undefined, Req) ->
  missing_parameter(Req);

logout(Token, Req) ->
  case auth_library:logout(Token) of
    ok ->
      cowboy_req:reply(200, Req);
    not_valid ->
      access_denied(Req)
  end.


reply(Data, Req) ->
  EchoJSON = jiffy:encode(Data, [force_utf8]),
  cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}], EchoJSON, Req).


missing_parameter(Req) ->
  cowboy_req:reply(400, [{<<"connection">>, <<"close">>}], <<"Missing a parameter!">>, Req).

access_denied(Req) ->
  cowboy_req:reply(403, [{<<"connection">>, <<"close">>}], Req).

method_not_allowed(Req) ->
  cowboy_req:reply(405, [{<<"connection">>, <<"close">>}], Req).

server_error(Req) ->
  cowboy_req:reply(500, [{<<"connection">>, <<"close">>}], Req).

terminate(_Reason, _Req, _State) ->
  ok.






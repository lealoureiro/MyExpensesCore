%%%-------------------------------------------------------------------
%%% @author leandroloureiro
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
  cowboy_req:reply(405, Req).


process(<<"/auth/login">>, PostVals, Req) ->
  Username = proplists:get_value(<<"username">>, PostVals),
  Password = proplists:get_value(<<"password">>, PostVals),
  login(Username, Password, Req);

process(<<"/auth/logout">>, PostVals, Req) ->
  Token = proplists:get_value(<<"token">>, PostVals),
  logout(Token, Req).


login(undefined, _, Req) ->
  missing_parameter(Req);

login(_, undefined, Req) ->
  missing_parameter(Req);

login(Username, Password, Req) ->
  case auth_library:login(Username, Password) of
    not_found ->
      cowboy_req:reply(403, Req);
    {Token, Id, Name} ->
      Data = {[{<<"token">>, Token}, {<<"id">>, Id}, {<<"name">>, Name}]},
      reply(Data, Req);
    _ ->
      cowboy_req:reply(500, Req)
  end.


logout(undefined, Req) ->
  missing_parameter(Req);

logout(Token, Req) ->
  case auth_library:logout(Token) of
    ok ->
      cowboy_req:reply(200, Req);
    not_valid ->
      cowboy_req:reply(403, Req)
  end.


reply(Data, Req) ->
  EchoJSON = jiffy:encode(Data, [force_utf8]),
  cowboy_req:reply(200, [{<<"content-type">>, <<"application/json; charset=utf-8">>}], EchoJSON, Req).


missing_parameter(Req) ->
  cowboy_req:reply(400, [], <<"Missing a parameter!">>, Req).


terminate(_Reason, _Req, _State) ->
  ok.






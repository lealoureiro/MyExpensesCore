%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 20. Mar 2016 21:21
%%%-------------------------------------------------------------------
-module(keys_handler).
-author("leandroloureiro").

%% API
-export([init/3]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([get_json/2]).
-export([process_post/2]).
-export([delete_resource/2]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"GET">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, process_post}], Req, State}.

get_json(Req, State) ->
  lager:log(info, self(), "Requested Key information ~n"),
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      cowboy_req:reply(400, [{<<"connection">>, <<"close">>}], Req),
      {halt, Req, State};
    {Token, _} ->
      lager:log(info, self(), "Client getting information for key ~s", [Token]),
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          Output = {[{<<"clientId">>, list_to_binary(uuid:uuid_to_string(ClientId))}]},
          {jiffy:encode(Output), Req, State};
        not_valid ->
          cowboy_req:reply(401, [{<<"connection">>, <<"close">>}], Req),
          {halt, Req, State}
      end
  end.

process_post(Req, State) ->
  lager:log(info, self(), "Requested new Key"),
  {ok, Body, Req1} = cowboy_req:body(Req),
  try
    {Data} = jiffy:decode(Body),
    Username = proplists:get_value(<<"username">>, Data),
    Password = proplists:get_value(<<"password">>, Data),
    case auth_library:login(Username, Password) of
      not_found ->
        cowboy_req:reply(401, [{<<"connection">>, <<"close">>}], Req1),
        {halt, Req1, State};
      invalid_password ->
        cowboy_req:reply(401, [{<<"connection">>, <<"close">>}], Req1),
        {halt, Req1, State};
      error ->
        lager:log(info, self(), "Problem to create Key for user ~s~n", [Username]),
        {false, Req, State};
      {Key, Id, Name} ->
        Output = {[{<<"key">>, Key}, {<<"clientId">>, Id}, {<<"clientName">>, Name}]},
        JSON = jiffy:encode(Output),
        Resp = cowboy_req:set_resp_body(JSON, Req),
        {true, Resp, State}
    end
  catch
    throw:{error, _} ->
      lager:log(info, self(), "Problem parsing the request!"),
      {false, Req, State}
  end.

delete_resource(Req, State) ->
  lager:log(info, self(), "Requested delete Key ~n"),
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      cowboy_req:reply(400, [{<<"connection">>, <<"close">>}], Req),
      {halt, Req, State};
    {Key, _} ->
      case auth_library:logout(Key) of
        ok ->
          {true, Req, State};
        not_valid ->
          {false, Req, State}
      end
  end.
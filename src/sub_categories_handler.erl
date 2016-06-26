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
-export([init/3]).
-export([allowed_methods/2]).
-export([is_authorized/2]).


init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
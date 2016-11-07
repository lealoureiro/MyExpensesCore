%%%-------------------------------------------------------------------
%%% @author leandro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 07. Nov 2016 9:19 PM
%%%-------------------------------------------------------------------
-module(tag_handler).
-author("leandro").

%% API
-export([
  init/3,
  allowed_methods/2,
  is_authorized/2,
  resource_exists/2,
  delete_resource/2
]).

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"DELETE">>, <<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:header(<<"authkey">>, Req) of
    {undefined, _} ->
      lager:log(info, self(), "Request Key missing!"),
      {{false, <<"Key">>}, Req, State};
    {Token, _} ->
      case auth_library:auth(Token) of
        {ok, ClientId} ->
          Req2 = cowboy_req:set_meta(<<"clientId">>, ClientId, Req),
          {Tag, _} = cowboy_req:binding(tag, Req),
          Req3 = cowboy_req:set_meta(<<"tag">>, Tag, Req2),
          {true, Req3, State};
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

resource_exists(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {Tag, _} = cowboy_req:meta(<<"tag">>, Req),
  lager:log(info, self(), "Client ~s accessing tag ~s", [uuid:uuid_to_string(ClientId), Tag]),
  Result = expenses_library:verify_tag(ClientId, Tag),
  {Result, Req, State}.

delete_resource(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {Tag, _} = cowboy_req:meta(<<"tag">>, Req),
  lager:log(info, self(), "Client ~s deleting tag ~s", [uuid:uuid_to_string(ClientId), Tag]),
  Result = expenses_library:delete_tag(ClientId, Tag),
  {Result, Req, State}.
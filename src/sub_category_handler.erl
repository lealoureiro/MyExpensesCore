%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2016 11:25 PM
%%%-------------------------------------------------------------------
-module(sub_category_handler).
-author("leandro").

%% API
-export([init/3,
  allowed_methods/2,
  is_authorized/2,
  resource_exists/2,
  delete_resource/2]).

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
          {Category, _} = cowboy_req:binding(categoryName, Req),
          Req3 = cowboy_req:set_meta(<<"category">>, Category, Req2),
          {SubCategory, _} = cowboy_req:binding(subCategoryName, Req),
          Req4 = cowboy_req:set_meta(<<"subCategory">>, SubCategory, Req3),
          {true, Req4, State};
        not_valid ->
          {{false, <<"Key">>}, Req, State}
      end
  end.

resource_exists(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {Category, _} = cowboy_req:meta(<<"category">>, Req),
  {SubCategory, _} = cowboy_req:meta(<<"subCategory">>, Req),
  lager:log(info, self(), "Client ~s accessing category ~s", [uuid:uuid_to_string(ClientId), Category]),
  Result = expenses_library:verify_sub_category(ClientId, Category, SubCategory),
  {Result, Req, State}.

delete_resource(Req, State) ->
  {ClientId, _} = cowboy_req:meta(<<"clientId">>, Req),
  {Category, _} = cowboy_req:meta(<<"category">>, Req),
  {SubCategory, _} = cowboy_req:meta(<<"subCategory">>, Req),
  lager:log(info, self(), "Client ~s deleting category ~s", [uuid:uuid_to_string(ClientId), Category]),
  Result = expenses_library:delete_sub_category(ClientId, Category, SubCategory),
  {Result, Req, State}.








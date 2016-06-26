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
-export([prepare_category_dictionary/2]).
-export([fill_sub_categories/2]).


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
  lager:log(info, self(), "Client ~s requested categories ~n", [uuid:uuid_to_string(ClientId)]),
  Categories = expenses_library:get_all_categories(ClientId),
  case Categories of
    [] ->
      {<<"[]">>, Req, State};
    [_ | _] ->
      Map = fun(Category) -> proplists:get_value(name, Category) end,
      CategoryList = lists:map(Map, Categories),
      InitialData = prepare_category_dictionary(CategoryList, dict:new()),
      SubCategories = expenses_library:get_all_subcategories(ClientId),
      case SubCategories of
        [] ->
          JSON = jiffy:encode({InitialData}),
          {JSON, Req, State};
        [_ | _] ->
          Result = dict:to_list(fill_sub_categories(SubCategories, InitialData)),
          JSON = jiffy:encode({Result}),
          {JSON, Req, State};
        _ ->
          {false, Req, State}
      end;
    _ ->
      lager:log(error, self(), "A problem occurred when fetching categories for user ~s", [uuid:uuid_to_string(ClientId)]),
      {false, Req, State}
  end.

prepare_category_dictionary([], Dict) ->
  Dict;

prepare_category_dictionary([H | T], D) ->
  prepare_category_dictionary(T, dict:store(H, [], D)).

fill_sub_categories([], Dict) ->
  Dict;

fill_sub_categories([SubCategory | T], Dict) ->

  Category = proplists:get_value(category_name, SubCategory),
  SubCategoryName = proplists:get_value(name, SubCategory),
  try
    SubCategoryList = dict:fetch(Category, Dict),
    fill_sub_categories(T, dict:store(Category, [SubCategoryName] ++ SubCategoryList, Dict))
  catch
    error:badarg ->
      lager:log(warning, self(), "Category ~s not found for sub category ~s!", [Category, SubCategoryName]),
      fill_sub_categories(T, Dict)
  end.
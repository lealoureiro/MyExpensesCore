%%%-------------------------------------------------------------------
%%% @author leandroloureiro
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2014 22:17
%%%-------------------------------------------------------------------
-module(auth_library).
-author("leandroloureiro").

-include("deps/mysql_client/include/mysql_types.hrl").
-include("include/sessions_records.hrl").

%% API
-export([login/2, auth/1, logout/1]).


login(Username, Password) ->
  HashPassword = crypto:hash(sha512, Password),
  HashPasswordString = hexstring(HashPassword),
  Con = datasource:get_connection(mysql_datasource),
  Statement = connection:get_prepared_statement_handle(Con, "SELECT id,username,name FROM Users WHERE username = ? AND password = ?"),
  {_, Rows} = connection:execute_statement(Con, Statement, [?MYSQL_TYPE_STRING, ?MYSQL_TYPE_STRING], [Username, HashPasswordString]),
  datasource:return_connection(mysql_datasource, Con),
  case Rows of
    [] ->
      not_found;
    [Entry | _] ->
      [Id, _, Name] = Entry,
      case createSession(Id) of
        {ok, Token} ->
          {Token, Id, Name};
        _ ->
          {error}
      end

  end.

createSession(Client_Id) ->
  Token_Bytes = crypto:strong_rand_bytes(64),
  Token = base64:encode(Token_Bytes),
  Timestamp = unixTimeStamp(),
  End = Timestamp + 300,
  Fun = fun() ->
    mnesia:write(#sessions{token = Token, client_id = Client_Id, started = Timestamp, ended = 0, last_heart_beat = End, valid = 1})
  end,
  Result = mnesia:transaction(Fun),
  case Result of
    {atomic, ok} ->
      {ok, Token};
    _ ->
      error
  end.

auth(Token) ->
  CheckTokenFun = fun() ->
    [Session] = mnesia:read(sessions, Token, read),
    case Session#sessions.valid of
      1 ->
        HeartBeat = unixTimeStamp(),
        SessionUpdated = Session#sessions{last_heart_beat = HeartBeat},
        mnesia:write(SessionUpdated),
        {ok, SessionUpdated};
      _ ->
        not_found
    end
  end,
  Sessions = mnesia:transaction(CheckTokenFun),
  case Sessions of
    {atomic, {ok, Session}} ->
      ClientId = Session#sessions.client_id,
      {ok, ClientId};
    _ ->
      {not_valid}
  end.


logout(Token) ->
  CheckTokenFun = fun() ->
    [Session] = mnesia:read(sessions, Token, read),
    case Session#sessions.valid of
      1 ->
        HeartBeat = unixTimeStamp(),
        SessionUpdated = Session#sessions{last_heart_beat = HeartBeat, valid = 0},
        mnesia:write(SessionUpdated);
      _ ->
        not_found
    end
  end,
  Sessions = mnesia:transaction(CheckTokenFun),
  case Sessions of
    {atomic, ok} ->
      ok;
    _ ->
      not_valid
  end.

hexstring(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X]));


hexstring(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X])).

unixTimeStamp() ->
  {Mega, Secs, _} = now(),
  Timestamp = Mega * 1000000 + Secs,
  Timestamp.



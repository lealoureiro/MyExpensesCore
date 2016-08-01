%%%-------------------------------------------------------------------
%%% @author Leandro Loureiro
%%% @copyright (C) 2014
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2014 22:17
%%%-------------------------------------------------------------------
-module(auth_library).
-author("leandroloureiro").

-include("sessions_records.hrl").
-include_lib("cqerl/include/cqerl.hrl").

%% API
-export([login/2,
  auth/1,
  logout/1,
  get_user_id/1,
  get_user_data/1,
  show_sessions/0,
  generate_token/1]).

login(Username, Password) when is_list(Username) ->
  login(list_to_binary(Username), Password);

login(Username, Password) ->
  HashPassword = crypto:hash(sha512, Password),
  HashPasswordString = list_to_binary(hex_string(HashPassword)),
  case get_user_id(Username) of
    {ok, ClientId} ->
      case get_user_data(ClientId) of
        {ok, Username, HashPasswordString, Name} ->
          case createSession(ClientId) of
            {ok, Token} ->
              {Token, list_to_binary(uuid:uuid_to_string(ClientId)), Name};
            _ ->
              lager:log(error, self(), "Failed to create session for user ~s!", [uuid:uuid_to_string(ClientId)]),
              system_error
          end;
        system_error ->
          lager:log(error, self(), "Failed get information for user ~s!", [uuid:uuid_to_string(ClientId)]),
          system_error;
        _ ->
          lager:log(warning, self(), "Invalid password attempt for user ~s!", [uuid:uuid_to_string(ClientId)]),
          invalid_password
      end;
    Other ->
      Other
  end.

get_user_id(Username) ->
  case cqerl:get_client({}) of
    {ok, Client} ->
      case cqerl:run_query(Client, #cql_query{statement = <<"SELECT user_id FROM users_by_username WHERE username = ?;">>, values = [{username, Username}]}) of
        {ok, Result} ->
          case cqerl:head(Result) of
            empty_dataset ->
              not_found;
            Row ->
              UserId = proplists:get_value(user_id, Row),
              {ok, UserId}
          end;
        {error, {Code, Description, _}} ->
          lager:log(error, self(), "Problem getting user id for username ~s: ~B - ~s", [Username, Code, Description]),
          system_error
      end;
    _ ->
      system_error
  end.

get_user_data(ClientId) ->
  case uuid:is_uuid(ClientId) of
    true ->
      case cqerl:get_client({}) of
        {ok, Client} ->
          case cqerl:run_query(Client, #cql_query{statement = <<"SELECT username,password,name FROM users WHERE user_id = ?;">>, values = [{user_id, ClientId}]}) of
            {ok, Result} ->
              case cqerl:head(Result) of
                empty_dataset ->
                  not_found;
                Row ->
                  Username = proplists:get_value(username, Row),
                  Password = proplists:get_value(password, Row),
                  Name = proplists:get_value(name, Row),
                  {ok, Username, Password, Name}
              end;
            {error, {Code, Description, _}} ->
              lager:log(error, self(), "Problem getting user data for user id ~s: ~B - ~s", [uuid:uuid_to_string(ClientId), Code, Description]),
              system_error
          end;
        _ ->
          system_error
      end;
    false ->
      system_error
  end.


createSession(ClientId) ->
  Token = generate_token(32),
  Timestamp = unixTimeStamp(),
  End = Timestamp + 300,
  Fun = fun() ->
    mnesia:write(#sessions{token = Token, client_id = ClientId, started = Timestamp, ended = 0, last_heart_beat = End, valid = 1})
        end,
  Result = mnesia:transaction(Fun),
  case Result of
    {atomic, ok} ->
      {ok, Token};
    _ ->
      system_error
  end.


generate_token(Size) ->
  TokenBytes = crypto:strong_rand_bytes(Size),
  base64:encode(TokenBytes).


auth(Token) when is_list(Token) ->
  auth(list_to_binary(Token));

auth(Token) when is_binary(Token) ->
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
      not_valid
  end.


logout(Token) ->
  CheckTokenFun = fun() ->
    [Session] = mnesia:read(sessions, Token, read),
    case Session#sessions.valid of
      1 ->
        HeartBeat = unixTimeStamp(),
        SessionUpdated = Session#sessions{last_heart_beat = HeartBeat, valid = 0, ended = 1},
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

hex_string(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X]));

hex_string(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X])).


unixTimeStamp() ->
  erlang:system_time(seconds).


show_sessions() ->
  Sessions = mnesia:async_dirty(fun() -> qlc:e(mnesia:table(sessions)) end),
  show_session_record(Sessions, 0).

show_session_record([H | Tail], Count) ->
  #sessions{token = T, client_id = C, started = S, ended = E, last_heart_beat = L, valid = V} = H,
  UUID = uuid:uuid_to_string(C),
  lager:log(info, self(), "User: ~s~nToken: ~s~nStarted: ~B    Last Activity: ~B     Ended: ~B     Valid: ~B~n~n", [UUID, T, S, L, E, V]),
  show_session_record(Tail, Count + 1);

show_session_record([], Count) ->
  lager:log(info, self(), "Sessions: ~B~n", [Count]),
  ok.

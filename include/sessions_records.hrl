%%%-------------------------------------------------------------------
%%% @author leandroloureiro
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Mar 2014 00:20
%%%-------------------------------------------------------------------
-author("leandroloureiro").


-record(sessions, {token, client_id, started, ended, last_heart_beat, valid}).
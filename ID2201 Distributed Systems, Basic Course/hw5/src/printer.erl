%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 12:24
%%%-------------------------------------------------------------------
-module(printer).
-author("emil").

-define(debug, false).

%% API
-export([var/2, msg/1, dbgvar/2, dbgmsg/1]).

msg(Msg) ->
  io:format(Msg ++ "~n").

var(Msg, Var) ->
  io:format(Msg ++ ": ~w~n", [Var]).

dbgmsg(Msg) ->
  if
    ?debug -> io:format(Msg ++ "~n");
    true -> ok
  end.

dbgvar(Msg, Var) ->
  if
    ?debug -> io:format(Msg ++ ": ~w~n", [Var]);
    true -> ok
  end.
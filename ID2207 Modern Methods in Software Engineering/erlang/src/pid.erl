%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2022 3:58 PM
%%%-------------------------------------------------------------------
-module(pid).
-author("emil").

%% API
-export([start/0]).
start() ->
  Pid = spawn(fun() -> server("Hello") end),
  Pid ! hello_process.

server(Message) ->
  io:fwrite("~p",[Message]).

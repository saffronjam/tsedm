%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2022 3:49 PM
%%%-------------------------------------------------------------------
-module(hello).
-author("emil").

%% API
-export([name/1, world/0]).

name(Name) ->
  io:fwrite(io:format("Hello, ~w~n", [Name])).

world() ->
  io:fwrite("Hello, World~n").
%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2022 10:00 AM
%%%-------------------------------------------------------------------
-module(generator).
-author("emil").

%% API
-export([start/0]).


start() ->
  [print(I) || I <- [1,2,3,4,5], I > 3],
  ok.

print(L) ->
  io:format("~w~n", [L]).
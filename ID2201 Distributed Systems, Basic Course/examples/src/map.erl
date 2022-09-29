%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2022 1:45 PM
%%%-------------------------------------------------------------------
-module(map).
-author("emil").

%% API
-export([create/2]).

create(Key, Value) ->
  M1 = #{Key=>Value, another=>item},
  io:fwrite("Map: ~w, Size: ~w~n", [M1, map_size(M1)]).
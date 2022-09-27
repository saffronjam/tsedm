%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 12:47
%%%-------------------------------------------------------------------
-module(gms3demo).
-author("emil").

-define(sleep, 1000).

%% API
-export([start_pc1/1, start_pc2/1, start_pc3/1, start_pc4/1, add_node/2]).

start_pc1(Module) ->
  % Create the leader initially
%%  Wrk = test:first(1, Module, ?sleep),
%%
%%  % Add slave
%%  test:add(2, Module, Wrk, ?sleep),
%%
%%  Wrk.
  test:more(4, gms3, 1000).


start_pc2(Module) ->
  test:add(3, Module, '<>@n142-p43', ?sleep).

start_pc3(Module) ->
  test:add(4, Module, '<>@n142-p43', ?sleep).


start_pc4(Module) ->
  test:add(5, Module, '<>@n142-p43', ?sleep).

add_node(Pc, Module) ->
  test:add(rand:uniform(1000) + 1000, Pc, Module, ?sleep).
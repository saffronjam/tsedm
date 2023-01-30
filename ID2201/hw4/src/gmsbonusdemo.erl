%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2022 12:47
%%%-------------------------------------------------------------------
-module(gmsbonusdemo).
-author("emil").


-define(sleep, 1000).
-define(module, gmsbonus).
-define(init_pc, {leader, pc1@home}).

%% API
-export([start_pc1/0, start_slave/0, add_node/2]).

start_pc1() ->
  % Create the leader initially
  Wrk = test:first(1, ?module, ?sleep),

  % Add slave
  test:add(2, ?module, ?init_pc, ?sleep),

  {ok, pc1, Wrk}.

start_slave() ->
  test:add(3, ?module, ?init_pc, ?sleep).


add_node(Pc, Module) ->
  test:add(rand:uniform(1000) + 1000, Pc, Module, ?sleep).
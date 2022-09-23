%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 11:26
%%%-------------------------------------------------------------------
-module(time).
-author("emil").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2, repr/1]).

zero() ->
  0.

inc(_, T) ->
  T + 1.

merge(Ti, Tj) ->
  max(Ti, Tj).

leq(Ti, Tj) ->
  Ti =< Tj.

clock(Nodes) ->
  InitializeNode =
    fun(Node, Acc) ->
      [{Node, zero()} | Acc]
    end,
  lists:foldl(InitializeNode, [], Nodes).

update(Node, Time, Clock) ->
  lists:keysort(2, lists:keyreplace(Node, 1, Clock, {Node, Time})).

safe(Time, Clock) ->
  [{_, LowestTime} | _] = Clock, % This works since the clock is sorted
  leq(Time, LowestTime).
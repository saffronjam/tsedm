%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Sep 2022 1:59 PM
%%%-------------------------------------------------------------------
-module(loop).
-author("emil").

%% API
-export([start_while/0, start_for/0, start_for_lambda/1]).

while(L) -> while(L, 0).
while([], _) -> ok;

while([Item | T], Acc) ->
  io:fwrite("item: ~w, count: ~w~n", [Item, Acc]),
  while(T, Acc + 1).

start_while() ->
  io:fwrite("While loop:~n"),
  X = [a, b, c, d],
  while(X).

for(0, _) ->
  [];

for(N, Fill) when N > 0 ->
  io:fwrite("Hello, at round ~w ~n", [N]),
  [Fill | for(N - 1, Fill)].

start_for() ->
  Fill = hello,
  Loops = 15,

  Result = for(Loops, Fill),
  io:fwrite("Result: ~w~n", [Result]),
  ok.

for_lambda(0, _) ->
  [];

for_lambda(N, Fn) when N > 0 ->
  [Fn(N) | for_lambda(N - 1, Fn)].

start_for_lambda(Fn) ->
  Loops = 5,

  Result = for_lambda(Loops, Fn),
  io:fwrite("Result: ~w~n", [Result]),
  ok.

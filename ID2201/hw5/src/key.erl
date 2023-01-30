%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 10:18
%%%-------------------------------------------------------------------
-module(key).
-author("emil").

-define(upper, 1000).

%% API
-export([generate/0, between/3, test/0]).

test() ->
  K1 = generate() - 1,
  IsBetweenNormal = between(K1, 0, ?upper / 2),
  IsBetweenReversed = between(K1, ?upper / 2, 0),
  IsBetweenReversedSame = between(K1, ?upper / 2, ?upper / 2),
  io:format("~w is between ~w and ~w? Answer: ~w~n", [K1, 0, ?upper / 2, IsBetweenNormal]),
  io:format("~w is between ~w and ~w? Answer: ~w~n", [K1, ?upper / 2, 0, IsBetweenReversed]),
  io:format("~w is between ~w and ~w? Answer: ~w~n", [K1, ?upper / 2, ?upper / 2, IsBetweenReversedSame]).

generate() ->
  rand:uniform(?upper).

between(Key, From, To) when From < To ->
  (Key > From) and (Key =< To);
between(Key, From, To) when From > To ->
  (Key > From) or (Key =< To);
between(_, From, To) when From == To ->
  true.
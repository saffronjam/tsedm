%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 12:38
%%%-------------------------------------------------------------------
-module(storage).
-author("emil").

%% API
-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() ->
  [].

add(Key, Value, Store) ->
  [{Key, Value} | Store].

lookup(Key, Store) ->
  lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
  Predicate = fun({Key, _}) -> key:between(Key, From, To) end,
  lists:partition(Predicate, Store).

merge(Entries, Store) ->
  lists:merge(Entries, Store).
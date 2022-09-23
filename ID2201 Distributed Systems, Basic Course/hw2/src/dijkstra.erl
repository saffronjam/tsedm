%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2022 10:38 AM
%%%-------------------------------------------------------------------
-module(dijkstra).
-author("emil").

%% API
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

entry(Node, Sorted) ->
  Result = lists:keyfind(Node, 1, Sorted),
  case Result of
    {_, Length, _} -> Length;
    false -> 0
  end.

replace(Node, N, Gateway, Sorted) ->
  WithoutNode = lists:keydelete(Node, 1, Sorted),
  WithNode = [{Node, N, Gateway} | WithoutNode],
  lists:keysort(2, WithNode).

update(Node, N, Gateway, Sorted) ->
  Length = entry(Node, Sorted),
  if
    N < Length -> replace(Node, N, Gateway, Sorted);
    true -> Sorted
  end.

route(Node, Table) ->
  Result = lists:keyfind(Node, 1, Table),
  case Result of
    false -> notfound;
    {_, Gateway} -> {ok, Gateway}
  end.

table(Gateways, Map) ->
  Nodes = map:all_nodes(Map),

  GenerateTable =
    fun(Node, Acc) ->
      [case lists:member(Node, Gateways) of
         true -> {Node, 0, Node};
         false -> {Node, inf, unknown}
       end | Acc]

    end,

  Folded = lists:foldl(GenerateTable, [], Nodes),
  InitialSorted = lists:keysort(2, Folded),

  Iterated = iterate(InitialSorted, Map, []),
  Iterated.


iterate([], _, Table) ->
  Table;

iterate([{_, inf, _} | _], _, Table) ->
  Table;

iterate([{Node, Hops, Through} | Sorted], Map, Table) ->
  UpdateClosestLinks =
    fun(X, Acc) ->
      update(X, Hops + 1, Through, Acc)
    end,

  % For all the reachable connections from this Node,
  % check if this Node can provide a better path
  Links = map:reachable(Node, Map),
  AfterHopsUpdate = lists:foldl(UpdateClosestLinks, Sorted, Links),
  iterate(AfterHopsUpdate, Map, [{Node, Through} | Table]).

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

  InitialSorted = lists:keysort(2,
    lists:foldl(
      fun(Node, Acc) ->
        [case lists:member(Node, Gateways) of
           true -> {Node, 0, Node};
           false -> {Node, inf, unknown}
         end | Acc]

      end, [], Nodes)
  ),

  iterate(InitialSorted, Map, []).


iterate([], _, Table) ->
  Table;

iterate([{_, inf, _}], _, Table) ->
  Table;

iterate(Sorted, Map, Table) ->
  [{To, Hops, Through} | Tail] = Sorted,
  Reachable = map:reachable(To, Map),

  case Reachable of
    %% If not comparable (not reachable), add directly to routing table
    [] -> iterate(Tail, Map, [{To, Through} | Table]);

    %% If reachable, update the route if hops + 1 (direct link) is better than existing
    Links ->
      UpdateClosestLinks = fun(Node, Acc) ->
        update(Node, Hops + 1, Through, Acc)
                           end,

      AfterHopsUpdate = lists:foldl(UpdateClosestLinks, Tail, Links),
      iterate(AfterHopsUpdate, Map, [{To, Through} | Table])
  end.


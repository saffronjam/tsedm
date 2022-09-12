%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Sep 2022 8:49 AM
%%%-------------------------------------------------------------------
-module(map).
-author("emil").

%% API
-export([new/0, update/3, reachable/2, all_nodes/1]).


new() ->
  [].

update(Node, Links, Map) ->
  WithoutNode = lists:keydelete(Node, 1, Map),
  WithoutNode ++ {Node, Links}.


reachable(Node, Map) ->
  Result = lists:keyfind(Node, 1, Map),
  case Result of
    false -> [];
    {_, Links} -> Links
  end.

all_nodes(Map) ->
  Collected = lists:foldl(fun({From, Links}, Acc) -> Acc ++ [From | Links] end, [], Map),
  Set = sets:from_list(Collected),
  sets:to_list(Set).


construct(Gateways) ->
  fun(X, Acc) ->
    case lists:member(X, Gateways) of
      true ->
        [{X, 0, X} | Acc];
      false ->
        [{X, inf, unknown} | Acc]
    end
  end.

table(Gateways, Map) ->
  NodesList = lists:foldl(construct(Gateways), [], map:all_nodes(Map)),
  Sorted = lists:keysort(2, NodesList),
  iterate(Sorted, Map, []).


route(Node, Table) ->
  case lists:keyfind(Node, 1, Table) of
    {_Node, Gw} ->
      {ok, Gw};
    false ->
      notfound
  end.

update(Node, New, Gateway, Sorted) ->
  Old = entry(Node, Sorted),
  case compare(New, Old) of
    false ->
      Sorted;
    true ->
      replace(Node, New, Gateway, Sorted)
  end.

iterate([], _Map, Table) ->
  Table;
iterate([{_, inf, _} | _Rest], _Map, Table) ->
  Table;
iterate([{Node, Hops, Gw} | Sorted], Map, Table) ->
  case map:reachable(Node, Map) of
    [] ->
      iterate(Sorted, Map, [{Node, Gw} | Table]);
    Nodes ->
      NewSorted = lists:foldl(fun(X, Acc) ->
        update(X, Hops + 1, Gw, Acc)
                              end,
        Sorted, Nodes),
      NewTable = [{Node, Gw} | Table],
      iterate(NewSorted, Map, NewTable)
  end.

entry(Node, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    {Node, Length, _Gw} ->
      Length;
    false ->
      0
  end.

replace(Node, N , Gateway, Sorted) ->
  case lists:keyfind(Node, 1, Sorted) of
    {Node, _, _Gateway} ->
      NewList = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
      lists:keysort(2, NewList);
    false ->
      Sorted
  end.

compare(New, Old) when New < Old ->
  true;
compare(_New, _Old) ->
  false.

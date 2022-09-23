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
  WithNode = [{Node, Links}|WithoutNode],
  WithNode.

reachable(Node, Map) ->
  Result = lists:keyfind(Node, 1, Map),
  case Result of
    false -> [];
    {_, Links} -> Links
  end.


all_nodes(Map) ->
  Collected = lists:foldl(fun({From, Links}, Acc) -> Acc ++ [From | Links] end, [], Map),
  lists:usort(Collected).
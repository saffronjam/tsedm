%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2022 08:32
%%%-------------------------------------------------------------------
-module(intf).
-author("emil").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).


new() ->
  [].

add(Name, Ref, Pid, Intf) ->
  WithoutName = lists:keydelete(Name, 1, Intf),
  Added = [{Name, Ref, Pid} | WithoutName],
  Added.

remove(Name, Intf) ->
  Removed = lists:keydelete(Name, 1, Intf),
  Removed.

lookup(Name, Intf) ->
  Result = lists:keyfind(Name, 1, Intf),
  case Result of
    false -> notfound;
    {_, _, Pid} -> {ok, Pid}
  end.

ref(Name, Intf) ->
  Result = lists:keyfind(Name, 1, Intf),
  case Result of
    false -> notfound;
    {_, Ref, _} -> {ok, Ref}
  end.

name(Ref, Intf) ->
  Result = lists:keyfind(Ref, 2, Intf),
  case Result of
    false -> notfound;
    {Name, _, _} -> {ok, Name}
  end.

list(Intf) ->
  Folded = lists:foldl(fun({Name, _, _}, Acc) -> [Name | Acc] end, [], Intf),
  Folded.

broadcast(_, []) ->
  ok;
broadcast(Message, [{_, _, Pid} | Intf]) ->
  Pid ! Message,
  broadcast(Message, Intf).
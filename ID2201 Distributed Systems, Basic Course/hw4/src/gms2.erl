%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 11:08 AM
%%%-------------------------------------------------------------------
-module(gms2).
-author("emil").

-define(timeout, 400).
-define(arghh, 100).

%% API
-export([start/1, start/2]).

%% Master
start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, [], [Master]).

%% Slave
start(Id, Grp) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  {ok, spawn_link(fun() -> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    {view, [Leader | Slaves], Group} ->
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

bcast(Id, Msg, List) ->
  lists:foreach(fun(Item) -> Item ! Msg, crash(Id) end, List).

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

leader(Id, Master, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, Slaves, Group) ->
  receive
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("Received DOWN from Master, electing new one...~n"),
      election(Id, Master, Slaves, Group);
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, Slaves, Group);
    {msg, Msg} ->
      Master ! Msg,
      slave(Id, Master, Leader, Slaves, Group);
    {view, [Leader | Slaves2], Group2} ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, Slaves2, Group2);
    stop ->
      ok
  end.

election(Id, Master, Slaves, [_ | Group]) ->
  Self = self(),
  case Slaves of
    [Self | Rest] ->
      bcast(Id, {view, Slaves, Group}, Rest),
      Master ! {view, Group},
      leader(Id, Master, Rest, Group);
    [Leader | Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, Rest, Group)
  end.
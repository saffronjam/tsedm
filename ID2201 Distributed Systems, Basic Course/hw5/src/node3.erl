%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 10:17
%%%-------------------------------------------------------------------
-module(node3).
-author("emil").

-define(Stabilize, 50).
-define(Timeout, 1000).

%% API
-export([start/1, start/2]).

start(Id) ->
  start(Id, nil).
start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, nil, storage:create()).

connect(Id, nil) ->
  % Connecting to ourself requires nothing more than returning self()
  {ok, {Id, nil, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      % Monitor the connected successor
      Ref = monitor(Peer),
      % Returning the received node, which is our sucessor
      {ok, {Skey, Ref, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n", [])
  end.

schedule_stabilize() ->
  printer:dbgmsg("Schedule stabilize"),
  timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor, Next, Store) ->
  %printer:dbgvar("Node loop", Id),
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Next, Store);
    {notify, New} ->
      {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, Next, UpdatedStore);
    {request, Peer} ->
      request(Peer, Predecessor, Successor),
      node(Id, Predecessor, Successor, Next, Store);
    {status, Pred, Nx} ->
      {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
      node(Id, Predecessor, Succ, Nxt, Store);
    stabilize ->
      printer:dbgvar("Stabilizing", Id),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Next, Store);
    probe ->
      create_probe(Id, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Next, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);
    {add, Key, Value, Qref, Client} ->
      UpdatedStore = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, UpdatedStore);
    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Next, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Next, Merged);
    {'DOWN', Ref, process, _, _} ->
      printer:var("Received DOWN from ", Ref),
      {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
      node(Id, Pred, Succ, Nxt, Store);
    stop ->
      ok
  end.

down(Ref, {_, Ref, _}, Successor, Next) ->
  {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
  Nref = monitor(Npid),
  Npid ! stabilize,
  % Hopefully a stabilize will occur before another node crashes...
  {Predecessor, {Nkey, Nref, Npid}, nil}.

add(Key, Value, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _, _}, {_, _, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.

create_probe(Id, {_, _, Spid}, Store) ->
  io:format("Node: ~w Store size: ~w ~n", [self(), length(Store)]),
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

forward_probe(Ref, T, Nodes, Id, {_, _, Spid}, Store) ->
  io:format("Node: ~w Store size: ~w ~n", [self(), length(Store)]),
  Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
  Now = erlang:system_time(micro_seconds),
  Delta = Now - T,
  printer:msg("-- Probe FINISHED --"),
  printer:var("Node count", length(Nodes)),
  printer:var("Duration (microseconds)", Delta),
  printer:msg("").

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      % Start monitoring this new predeccessor
      Nref = monitor(Npid),
      % If predeccessor is nil, there was no being monitored before, so no demonitor is needed
      {{Nkey, Nref, Npid}, Keep};
    {Pkey, Pref, _} ->
      % Don't need a special case for self reference since we handle that case in key:between
      case key:between(Nkey, Pkey, Id) of
        true ->
          % Give part of the store to the new node and monitor it since it is a predeccessor
          Keep = handover(Id, Store, Nkey, Npid),
          Nref = monitor(Npid),
          % Then stop monitoring the old predecessor as Npid will do that instead
          drop(Pref),

          {{Nkey, Nref, Npid}, Keep};
        false -> {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  {Rest, Keep} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

request(Peer, Predecessor, Successor) ->
  % We drop Sref here since it is sent as the next node, and they are not monitored
  {Skey, _, Spid} = Successor,
  case Predecessor of
    nil ->
      Peer ! {status, nil, {Skey, Spid}};
    {Pkey, _, Ppid} ->

      Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
  end.

stabilize({_, _, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Next, Id, Successor) ->
  {Skey, _, Spid} = Successor,
  case Pred of
    nil ->
      % Just attach to the leaf
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    % Point to ourselves, we do nothing
    {Id, _} -> {Successor, Next};
    % Successor point to itself, notify about our existance
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      {Successor, Next};
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {request, self()},

          {Skey, Sref, Spid} = Successor,
          % Monitor our new successor
          Xref = monitor(Xpid),
          % Demonitor the old sucessor
          drop(Sref),

          {{Xkey, Xref, Xpid}, {Skey, Spid}};
        false ->
          Spid ! {notify, {Id, self()}},
          {Successor, Next}
      end
  end.

monitor(Pid) ->
  printer:var("Started monitor [By, On]", [self(), Pid]),
  erlang:monitor(process, Pid).
drop(nil) ->
  ok;
drop(Pid) ->
  printer:var("Dropped monitor [By, On]", [self(), Pid]),
  erlang:demonitor(Pid, [flush]).
%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2022 10:17
%%%-------------------------------------------------------------------
-module(node1).
-author("emil").

-define(Stabilize, 1000).
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
  node(Id, Predecessor, Successor).

connect(Id, nil) ->
  % Connecting to ourself requires nothing more than returning self()
  {ok, {Id, self()}};
connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      % Returning the received node, which is our sucessor
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n", [])
  end.

schedule_stabilize() ->
  printer:dbgmsg("Schedule stabilize"),
  timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor) ->
  printer:dbgvar("Node loop", Id),
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);
    {notify, New} ->
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ);
    stabilize ->
      stabilize(Successor),
      node(Id, Predecessor, Successor);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)
  end.

create_probe(Id, {_, Spid}) ->
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

forward_probe(Ref, T, Nodes, Id, {_, Spid}) ->
  Spid ! {probe, Ref, [Id | Nodes], T}.

remove_probe(T, Nodes) ->
  Now = erlang:system_time(micro_seconds),
  Delta = Now - T,
  printer:msg("-- Probe finished -- ~n"),
  printer:var("Nodes", Nodes),
  printer:var("Duration (microseconds)", Delta).

notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil -> {Nkey, Npid};
    {Pkey, _} ->
      % Don't need a special case for self reference since we handle that case in key:between
      case key:between(Nkey, Pkey, Id) of
        true -> {Nkey, Npid};
        false -> Predecessor
      end
  end.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

stabilize({_, Spid}) ->
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  printer:dbgvar("Stabilizing", Id),
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    % Point to ourselves, we do nothing
    {Id, _} -> Successor;
    % Successor point to itself, notify about our existance
    {Skey, _} ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true -> Xpid ! {request, self()}, Pred;
        false -> Spid ! {notify, {Id, self()}}, Successor
      end
  end.
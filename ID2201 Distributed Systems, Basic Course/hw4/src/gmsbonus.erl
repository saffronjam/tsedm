%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Sep 2022 11:08 AM
%%%-------------------------------------------------------------------
-module(gmsbonus).
-author("emil").

-define(timeout, 400).
-define(okrec_timeout, 100).
-define(arghh, 10000).
-define(arghh_msg, 10).

%% API
-export([start/1, start/2]).

%% Master
start(Id) ->
  Rnd = rand:uniform(1000),
  Self = self(),
  MasterProc = spawn_link(fun() -> init(Id, Rnd, Self) end),
  register(leader, MasterProc),
  {ok, MasterProc}.

init(Id, Rnd, Master) ->
  random:seed(Rnd, Rnd, Rnd),
  leader(Id, Master, 0, [], [Master]).

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
    {view, N, [Leader | Slaves], Group} ->
      okrec(Leader),
      Master ! {view, Group},
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves], Group}, Slaves, Group)
  after ?timeout ->
    Master ! {error, "no reply from leader"}
  end.

bcast(Id, Msg, List) ->
  lists:foreach(fun(Item) -> ucast(Item, Msg), crash(Id) end, List).

% Unicast function to wrap behavior of lost messages
ucast(To, Msg) ->
  send(To, Msg),
  Result = wait_for_okrec(),
  case Result of
    ok -> ok;
    % Resend if we had a timeout
    timeout ->
      io:format("message timeout, resending~n"),
      ucast(To, Msg)
  end.

send(To, Msg) ->
  Failed = random:uniform(?arghh_msg) == ?arghh_msg,
  if
  % Opsie we failed to send the message, don't do anything
    Failed -> ok;
  % Just a normal send
    true -> To ! Msg
  end.

wait_for_okrec() ->
  receive
    {okrec} ->
      ok
  after ?okrec_timeout ->
    timeout
  end.

crash(Id) ->
  case random:uniform(?arghh) of
    ?arghh ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.

leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N, Msg}, Slaves),
      Master ! Msg,
      % Increment N for next iteration
      leader(Id, Master, N + 1, Slaves, Group);
    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N, [self() | Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      % Increment N for next iteration
      leader(Id, Master, N + 1, Slaves2, Group2);
    stop ->
      ok
  end.

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  io:format("received message ~w  master:~w leader:~w ~n", [N, Master, Leader]),
  receive
    {'DOWN', _Ref, process, Leader, _Reason} ->
      io:format("received DOWN from Master, electing new one...~n"),
      election(Id, Master, N, Last, Slaves, Group);
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);
  % Throw away old messages
    {msg, I, _} when I < N ->
      okrec(Leader),
      slave(Id, Master, Leader, N, Last, Slaves, Group);
    {msg, N, Msg} ->
      okrec(Leader),
      Master ! Msg,
      % Increment N for next iteration
      slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Slaves, Group);
    {view, N, [Leader | Slaves2], Group2} ->
      okrec(Leader),
      Master ! {view, Group2},
      % Increment N for next iteration
      slave(Id, Master, Leader, N + 1, {view, N, [Leader | Slaves2], Group2}, Slaves2, Group2);

    stop ->
      ok
  end.

okrec(Leader) ->
  Leader ! {okrec}.

election(Id, Master, N, Last, Slaves, [_ | Group]) ->
  Self = self(),
  case Slaves of
    [Self | Rest] ->
      % Before electing ourself as a leader,
      %   send our last message in case it was not received by the others when we crashed
      %   This works since, if we are elected as leader, we were first in the list
      %   And we must then have received the "most" messages out of all the nodes
      % Hopefully one message back is enough...
      bcast(Id, Last, Rest),

      % Change name of process to leader, when I die later it will be released
      register(leader, self()),

      bcast(Id, {view, N, Slaves, Group}, Rest),
      Master ! {view, Group},
      % Increment N for next iteration
      leader(Id, Master, N + 1, Rest, Group);
    [Leader | Rest] ->
      % Monitor new leader
      erlang:monitor(process, Leader),

      % Don't increment N since no messages were sent
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.


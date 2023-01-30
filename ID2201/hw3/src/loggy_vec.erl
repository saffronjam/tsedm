%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 10:06
%%%-------------------------------------------------------------------

-module(loggy_vec).
-author("emil").

%% API
-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  Clock = time_vec:clock(Nodes),
  Queue = [],
  loop(Clock, Queue, 0).

loop(OldClock, OldQueue, LargestQueueSize) ->
  receive
    {log, From, Time, Msg} ->


      %% Update clock
      Clock = time_vec:update(From, Time, OldClock),

      %% Store message
      Queue = store_message(From, Time, Msg, OldQueue),

      %% Then go through list of all messages that are now ready to be printed
      Rest = go_through_queue(Clock, Queue, LargestQueueSize),

      loop(Clock, Rest, max(length(Rest), LargestQueueSize));
    stop ->
      ok
  end.

log(From, Time, Msg, QueueSize, LargestQueueSize) ->
  io:format("log: ~w ~w ~p (Queue size: ~w | Max: ~w)~n", [Time, From, Msg, QueueSize, LargestQueueSize]).

go_through_queue(Clock, Queue, LargestQueueSize) ->
  {Sendable, NotSendable} = lists:splitwith(
    fun({_, Time, _}) ->
      time_vec:safe(Time, Clock)
    end, Queue),

  lists:foreach(fun({From, Time, Msg}) -> log(From, Time, Msg, length(Queue), LargestQueueSize) end, Sendable),

  NotSendable.


store_message(From, Time, Msg, Queue) ->
  NewQueue = [{From, Time, Msg} | Queue],
  lists:keysort(2, NewQueue).
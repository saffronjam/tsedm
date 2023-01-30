%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 10:08
%%%-------------------------------------------------------------------
-module(worker).
-author("emil").

%% API
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.
init(Name, Log, Seed, Sleep, Jitter) ->

  random:seed(Seed, Seed, Seed),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, time:zero());
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time) ->
  Wait = random:uniform(Sleep),
  receive
    {msg, IncomingTime, Msg} ->
      Newest = time:merge(Time, IncomingTime),
      IncreasedTime = time:inc(Name, Newest),
      Log ! {log, Name, IncreasedTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, IncreasedTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
    % Message and peer
    Selected = select(Peers),
    Message = {hello, random:uniform(100)},

    IncreasedTime = time:inc(Name, Time),

    %% Send
    Selected ! {msg, IncreasedTime, Message},

    jitter(Jitter),

    %% Log
    Log ! {log, Name, IncreasedTime, {sending, Message}},

    loop(Name, Log, Peers, Sleep, Jitter, IncreasedTime)
end.

select(Peers) ->
  lists:nth(random:uniform(length(Peers)), Peers).
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).

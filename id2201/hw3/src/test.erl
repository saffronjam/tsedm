%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 10:28
%%%-------------------------------------------------------------------
-module(test).
-author("emil").

%% API
-export([run/2, run_vec/2]).

run(Sleep, Jitter) ->
  Log = loggy:start([john, paul, ringo, george]),

  A = worker:start(john, Log, 13, Sleep, Jitter),
  B = worker:start(paul, Log, 23, Sleep, Jitter),
  C = worker:start(ringo, Log, 36, Sleep, Jitter),
  D = worker:start(george, Log, 49, Sleep, Jitter),

  worker:peers(A, [B, C, D]),
  worker:peers(B, [A, C, D]),
  worker:peers(C, [A, B, D]),
  worker:peers(D, [A, B, C]),

  timer:sleep(5000),

  loggy:stop(Log),
  worker:stop(A),
  worker:stop(B),
  worker:stop(C),
  worker:stop(D).

run_vec(Sleep, Jitter) ->
  Log = loggy_vec:start([john, paul, ringo, george]),

  A = worker_vec:start(john, Log, 13, Sleep, Jitter),
  B = worker_vec:start(paul, Log, 23, Sleep, Jitter),
  C = worker_vec:start(ringo, Log, 36, Sleep, Jitter),
  D = worker_vec:start(george, Log, 49, Sleep, Jitter),

  worker_vec:peers(A, [B, C, D]),
  worker_vec:peers(B, [A, C, D]),
  worker_vec:peers(C, [A, B, D]),
  worker_vec:peers(D, [A, B, C]),

  timer:sleep(5000),

  loggy_vec:stop(Log),
  worker_vec:stop(A),
  worker_vec:stop(B),
  worker_vec:stop(C),
  worker_vec:stop(D).

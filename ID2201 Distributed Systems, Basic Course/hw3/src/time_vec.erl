%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2022 14:09
%%%-------------------------------------------------------------------
-module(time_vec).
-author("emil").

%% API
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2, repr/1]).

zero() ->
  [].

inc(Name, Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Count} ->
      lists:keyreplace(Name, 1, Time, {Name, Count + 1});
    false ->
      [{Name, 1} | Time]
  end.

merge([], Time) ->
  Time;
merge([{Name, Ti} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      [{Name, max(Ti, Tj)} | merge(Rest, lists:keydelete(Name, 1, Time))];
    false ->
      [{Name, Ti} | merge(Rest, Time)]
  end.

leq([], _) ->
  true;
leq([{Name, Ti} | Rest], Time) ->
  case lists:keyfind(Name, 1, Time) of
    {Name, Tj} ->
      if
        Ti =< Tj -> leq(Rest, lists:keydelete(Name, 1, Time));
        true -> false
      end;
    false -> false
  end.

clock(_) ->
  [].

update(From, Time, Clock) ->
  {Node, Count} = lists:keyfind(From, 1, Time),
  case lists:keyfind(From, 1, Clock) of
    {From, _} ->
      lists:keyreplace(From, 1, Clock, {Node, Count});
    false ->
      [{Node, Count} | Clock]
  end.

safe(Time, Clock) ->
  leq(Time, Clock).
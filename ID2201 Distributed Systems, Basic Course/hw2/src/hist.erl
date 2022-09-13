%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2022 08:46
%%%-------------------------------------------------------------------
-module(hist).
-author("emil").

%% API
-export([new/1, update/3]).

new(Name) ->
  [{Name, 0}].

update(Node, N, History) ->
  Result = lists:keyfind(Node, 1, History),

  case Result of
    false ->
      Updated = [{Node, N} | History],
      {new, Updated};
    {_, Oldness} ->
      if
        N > Oldness ->
          WithoutNode = lists:keydelete(Node, 1, History),
          Updated = [{Node, N} | WithoutNode],
          {new, Updated};
        true -> old
      end
  end.
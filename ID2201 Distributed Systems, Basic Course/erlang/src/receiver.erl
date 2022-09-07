%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Sep 2022 4:14 PM
%%%-------------------------------------------------------------------
-module( receiver).
-author("emil").

%% API
-export([hello/0]).

hello() ->
  receive
    X -> io:format("aaa! surprise, a message: ~s~n", [X])
  end.

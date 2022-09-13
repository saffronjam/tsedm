%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2022 11:21
%%%-------------------------------------------------------------------
-module(helpers).
-author("emil").

%% API
-export([print/1]).


print(Something) ->
  io:fwrite("~w~n", [Something]).
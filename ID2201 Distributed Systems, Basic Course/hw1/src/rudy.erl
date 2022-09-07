%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2022 12:28
%%%-------------------------------------------------------------------
-module(rudy).
-author("emil").

%% API
-export([start/1, start/2, stop/0, fib/1, request/1, reply/1]).

start(Port) ->
  start(Port, 1).

start(Port, N) ->
  register(rudy4, spawn(fun() -> init(Port, N) end)).

stop() ->
  rudy4 ! stop.

init(Port, N) ->
  case gen_tcp:listen(Port, [list, {active, false}, {reuseaddr, true}]) of
    {ok, Listen} ->
      handlers(Listen, N),
      super();
    {error, Error} ->
      io:format("rudy: initialization failed: ~w~n", [Error]),
      error
  end.

super() ->
  receive
    stop ->
      ok
  end.

handlers(Listen, N) ->
  case N of
    0 ->
      ok;
    N ->
      spawn(fun() -> handler(Listen, N) end),
      handlers(Listen, N - 1)
  end.



handler(Listen, I) ->
  %%io:format("rudy: waiting for request~n", []),
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      io:format("rudy ~w: received request~n", [I]),
      request(Client),
      handler(Listen, I);
    {error, Error} ->
      io:format("rudy: error ~w~n", [Error]),
      error
  end.

request(Client) ->
  %%io:format("rudy: reading request from ~w~n", [Client]),
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response),
      gen_tcp:close(Client);
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error]),
      ok
  end.




reply({{get, URI, _}, _, _}) ->
  timer:sleep(40),
  %%fib(30),

  %% http:ok("<html><head><title>Rudy</title></head><body>You tried to enter: " ++ URI ++ "</body></html>").

  Result = rudyfile:get_file_by_uri(URI),
  case Result of
    {ok, Content, Size, ContentType} ->
      http:ok_file(ContentType, Content, Size);
    {error, _} ->
      http:not_found()
  end.



%%  http:ok("<html><head><title>Rudy</title></head><body>You tried to enter: " ++ URI ++ "</body></html>").


fib(N) ->
  if
    N == 0 ->
      0;
    N == 1 ->
      1;
    true -> fib(N - 1) + fib(N - 2)
  end.


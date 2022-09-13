%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Sep 2022 09:02
%%%-------------------------------------------------------------------
-module(routy).
-author("emil").

%% API
-export([start/2, stop/1, statusrequest/1]).

start(Reg, Name) ->
  register(Reg, spawn(fun() -> init(Name) end)).

stop(Node) ->
  Node ! stop,
  unregister(Node).

init(Name) ->
  Intf = intf:new(),
  Map = map:new(),
  Table = dijkstra:table(Intf, Map),
  Hist = hist:new(Name),
  router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
  receive
    {add, Node, Pid} ->
      prettyprint(Name, Node, "ADD"),
      Ref = erlang:monitor(process, Pid),
      Intf1 = intf:add(Node, Ref, Pid, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {remove, Node} ->
      prettyprint(Name, Node, "REM"),
      {ok, Ref} = intf:ref(Node, Intf),
      erlang:demonitor(Ref),
      Intf1 = intf:remove(Node, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    broadcast ->
      Message = {links, Name, N, intf:list(Intf)},
      prettyprint(Name, system, "BRC", Message),
      intf:broadcast(Message, Intf),
      router(Name, N + 1, Hist, Intf, Table, Map);

    update ->
      UpdatedTable = dijkstra:table(intf:list(Intf), Map),
      router(Name, N, Hist, Intf, UpdatedTable, Map);

    {'DOWN', Ref, process, _, _} ->
      {ok, Down} = intf:name(Ref, Intf),
      prettyprint(Name, Down, "EXT"),
      Intf1 = intf:remove(Down, Intf),
      router(Name, N, Hist, Intf1, Table, Map);

    {links, Node, R, Links} ->
      prettyprint(Name, Node, "LNK", Links),
      case hist:update(Node, R, Hist) of
        {new, Hist1} ->
          intf:broadcast({links, Node, R, Links}, Intf),
          Map1 = map:update(Node, Links, Map),
          router(Name, N, Hist1, Intf, Table, Map1);
        old ->
          router(Name, N, Hist, Intf, Table, Map);
        notfound ->
          router(Name, N, Hist, Intf, Table, Map)
      end;

    {route, Name, From, Message} ->
      io:format("~w: received message ~w ~n", [Name, Message]),
      router(Name, N, Hist, Intf, Table, Map);

    {route, To, From, Message} ->
      prettyprint(Name, From, "ROU", Message),
      case dijkstra:route(To, Table) of
        {ok, Gw} ->
          case intf:lookup(Gw, Intf) of
            {ok, Pid} ->
              Pid ! {route, To, From, Message};
            notfound ->
              io:fwrite("No Gw pid found. Interfaces: ~w~n", [Intf]),
              ok
          end;
        notfound ->
          io:fwrite("No dijkstra path found. Table: ~w ~n", [Table]),
          ok
      end,
      router(Name, N, Hist, Intf, Table, Map);

    {send, To, Message} ->
      self() ! {route, To, Name, Message},
      router(Name, N, Hist, Intf, Table, Map);

    {status, From} ->
      From ! {status, {Name, N, Hist, Intf, Table, Map}},
      router(Name, N, Hist, Intf, Table, Map);
    stop ->
      ok
  end.

statusrequest(Pid) ->
  MyPid = self(),
  Pid ! {status, MyPid},
  receive
    {status, Status} ->
      prettyprint(Pid, system, "STATUS", Status)
  end.

prettyprint(Receiver, Sender, Type) ->
  io:fwrite("[~s] (~w -> ~w) ~n", [Type, Sender, Receiver]).
prettyprint(Receiver, Sender, Type, Message) ->
  io:fwrite("[~s] (~w -> ~w) ~w~n", [Type, Sender, Receiver, Message]).
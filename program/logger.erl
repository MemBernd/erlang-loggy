-module(logger).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    loop(time:clock(Nodes), []).

loop(Clock, Queue)  ->
    receive
        {log, From, Time, Msg} ->
            {NewClock, NewQueue} = log(From, Time, Msg, Clock, Queue),
            loop(NewClock, NewQueue);
        stop ->
            io:format("Emptying Queue...~n"),
            lists:foreach(fun({Time, From, Message, _}) ->
                    io:format("log: ~w ~w ~p~n", [Time, From, Message])
                end,
                lists:keysort(1, Queue)),
            ok
    end.

log(From, Time, Msg, Clock, Queue) ->
    %io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
    NewClock = vect:update(From, Time, Clock),
    %sorted the hold back queue based on the sum of all the values in the timestamp
    Sum = lists:foldl(fun({_, X}, Res) -> Res +X end, 0, Time),
    NewQueue = lists:keysort(4,[{Time, From, Msg, Sum} | Queue]),
    io:format("~nLooking up Queue With clock: ~p~n", [NewClock]),
    EndQueue = lists:foldl(fun(Elem, TempQueue) -> processQueue(Elem, NewClock, TempQueue) end, [], NewQueue),
    {NewClock, EndQueue}.

processQueue({Time, From, Message, Sum}, Clock, Queue) ->
    case vect:safe(Time, Clock) of
        true ->
            io:format("log: ~w ~w ~p~n", [Time, From, Message]),
            Queue;
        false ->
            [{Time, From, Message, Sum} | Queue]
    end.

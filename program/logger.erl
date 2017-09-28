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
            lists:foreach(fun({Time, From, Message}) ->
                    io:format("log: ~w ~w ~p~n", [Time, From, Message])
                end,
                lists:keysort(1, Queue)),
            ok
    end.

log(From, Time, Msg, Clock, Queue) ->
    %io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
    NewClock = vect:update(From, Time, Clock),
    %io:format("clock ~p~n", [NewClock]),
    NewQueue = [{Time, From, Msg} | Queue],
    io:format("~nLooking up Queue With clock: ~p~n", [NewClock]),
    EndQueue = lists:foldl(fun(Elem, TempQueue) -> processQueue(Elem, NewClock, TempQueue) end, [], NewQueue),
    {NewClock, EndQueue}.

processQueue({Time, From, Message}, Clock, Queue) ->
    %io:format("checking: ~nTime: ~w~nFrom: ~w~nMessage: ~w~n", [Time, From, Message]),
    %if Queue == [] ->
    %        io:format("Found messages which can be printed.~n")
    %end,
    case vect:safe(Time, Clock) of
        true ->
            io:format("log: ~w ~w ~p~n", [Time, From, Message]),
            Queue;
        false ->
            [{Time, From, Message} | Queue]
    end.

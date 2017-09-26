-module(time).
-export([zero/0, inc/2, merge/2, leq/2, newTime/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    erlang:max(Ti, Tj).

leq(Ti, Tj) ->
    Ti < Tj.

newTime(TimeSelf, TimeReceived) ->
    inc(static, merge(TimeSelf, TimeReceived)).

clock(Nodes) ->
    lists:foldl(fun(X, Acc) -> [{X, 0}|Acc] end, [], Nodes).

update(Node, Time, Clock) ->
    {_, ClockRetrieved} = lists:keyfind(Node, 1, Clock),
    case leq(ClockRetrieved, Time) of
        true ->
            lists:keyreplace(Node, 1, Clock, {Node, Time});
        false ->
            Clock
    end.

safe(_, []) ->
    true;
safe(TimeGiven, [{_,TimeRecord}|Rest]) when TimeGiven =< TimeRecord ->
    safe(TimeGiven, Rest);
safe(_,_) ->
    false.

-module(vect).
-export([zero/0, inc/2, merge/2, leq/2, newTime/3, clock/1, update/3, safe/2]).

zero() ->
    [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, TimeRetrieved} ->
            %io:format("~n Name:~w~nTimeretrieved~w~n", [Name, TimeRetrieved]),
            lists:keyreplace(Name, 1, Time, {Name, TimeRetrieved + 1});
        false ->
            [{Name, 1} | Time]
    end.

merge([], Time) ->
    Time;
merge([{Name, Ti} | Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, max(Ti,Tj)} |merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti} | merge(Rest, Time)]
    end.

leq([],_) ->
    true;
leq([{NameI, TI} | RestI], CJ) ->
    case lists:keyfind(NameI, 1, CJ) of
        {NameJ, TJ} ->
            Tcompare = TJ;
        false ->
            Tcompare = 0
    end,
    if TI =< Tcompare ->
            leq(RestI, CJ);
        true ->
            false
    end.

newTime(Name, TimeSelf, TimeReceived) ->
    inc(Name, merge(TimeSelf, TimeReceived)).




%create a vector with length N
createVector(N) ->
    createVector(N, []).
createVector(0, List) ->
    List;
createVector(N, List) ->
    createVector(N-1, [0 | List]).


%logger functions

clock(Nodes) ->
    %lists:foldl(fun(X, Acc) -> [{X, 0}|Acc] end, [], Nodes).
    [].

update(From, Time, Clock) ->
    TimeStamp = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, _} ->
            lists:keyreplace(From, 1, Clock, TimeStamp);
        false ->
                [TimeStamp | Clock]
end.

safe([], _) ->
    true;
safe([{NameI, TI}| RestI], Clock) ->
    case lists:keyfind(NameI, 1, Clock) of
        {_, TJ} ->
            if TI =< TJ ->
                    safe(RestI, Clock);
                true ->
                    false
            end;
        false ->
            false
    end.

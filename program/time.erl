-module(time).
-export([zero/0, inc/2, merge/2, leq/2]).

zero() ->
    0.

inc(Name, T) ->
    T + 1.

merge(Ti, Tj) ->
    erlang:max(Ti, Tj).

leq(Ti, Tj) ->
    Ti < Tj.

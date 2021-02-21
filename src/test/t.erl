-module(t).

-export([a/1]).
-export([s/0]).

a(A) ->
    A.

s() ->
    F = fun a/1,
    F(1).
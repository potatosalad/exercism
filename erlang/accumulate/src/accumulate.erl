-module(accumulate).

-export([accumulate/2]).

accumulate(Fn, Ls) ->
    [Fn(L) || L <- Ls].

-module(strain).

-export([keep/2, discard/2]).

keep(Fn, List) ->
    [Element || Element <- List, Fn(Element)].

discard(Fn, List) ->
    [Element || Element <- List, not Fn(Element)].

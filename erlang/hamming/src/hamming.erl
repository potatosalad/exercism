-module(hamming).

-export([distance/2]).

distance(Strand1, Strand2) when length(Strand1) =:= length(Strand2) ->
    distance(Strand1, Strand2, 0);
distance(_, _) ->
    {error, "left and right strands must be of equal length"}.

%% @private
distance([], [], C) ->
    C;
distance([H | T1], [H | T2], C) ->
    distance(T1, T2, C);
distance([_ | T1], [_ | T2], C) ->
    distance(T1, T2, C + 1).

-module(collatz_conjecture).

-export([steps/1]).

-spec steps(pos_integer()) -> non_neg_integer();
           (any()) -> {error, string()}.
steps(N) when is_integer(N) andalso N > 0 ->
    collatz(N, 0);
steps(_) ->
    {error, "Only positive numbers are allowed"}.

%% @private
collatz(1, Steps) ->
    Steps;
collatz(N, Steps) when N rem 2 =:= 0 ->
    collatz(N div 2, Steps + 1);
collatz(N, Steps) ->
    collatz(3 * N + 1, Steps + 1).

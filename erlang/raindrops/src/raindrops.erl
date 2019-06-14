-module(raindrops).

-export([convert/1]).

-spec convert(pos_integer()) -> string().
convert(N) when is_integer(N) andalso N > 0 ->
    case lists:flatten([pling(N), plang(N), plong(N)]) of
        [] ->
            erlang:integer_to_list(N);
        R = [_ | _] ->
            R
    end.

%% @private
pling(N) when N rem 3 =:= 0 -> "Pling";
pling(_) -> [].

%% @private
plang(N) when N rem 5 =:= 0 -> "Plang";
plang(_) -> [].

%% @private
plong(N) when N rem 7 =:= 0 -> "Plong";
plong(_) -> [].

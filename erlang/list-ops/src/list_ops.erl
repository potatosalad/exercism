-module(list_ops).

-compile({no_auto_import, [length/1]}).

-export([append/2, concat/1, filter/2, length/1, map/2, foldl/3, foldr/3,
         reverse/1]).

append(List1, List2) ->
	reverse(reverse(List1), List2).

concat(List) ->
	concat(List, []).

%% @private
concat([], Acc) ->
	reverse(Acc);
concat([[] | T2], Acc) ->
	concat(T2, Acc);
concat([[H | T1] | T2], Acc) ->
	concat([T1 | T2], [H | Acc]).

filter(Function, List) ->
	[Element || Element <- List, Function(Element)].

length([]) ->
	0;
length([_ | T]) ->
	1 + length(T).

map(_Function, []) ->
	[];
map(Function, [H | T]) ->
	[Function(H) | map(Function, T)].

foldl(_Function, Start, []) ->
	Start;
foldl(Function, Start, [H | T]) ->
	foldl(Function, Function(H, Start), T).

foldr(_Function, Start, []) ->
	Start;
foldr(Function, Start, [H | T]) ->
	Function(H, foldr(Function, Start, T)).

reverse([]) ->
	[];
reverse([H | T]) ->
	reverse(T, [H]).

%% @private
reverse([], Acc) ->
	Acc;
reverse([H | T], Acc) ->
	reverse(T, [H | Acc]).

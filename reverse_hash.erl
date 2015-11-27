-module(reverse_hash).

-export([solve/4]).

% Stands for math:pow(2, 27) - 1 but performs better?!  
-define(MAX, 134217727). 

solve(Fun, Inputs, P, Schedulers) -> 
	Parent = self(),
	Values = ?MAX div Schedulers,
	R = [spawn_link(fun() -> reverse_hash(Fun, Inputs, Parent, N * Values + 1) end) || N <- lists:seq(0, Schedulers - 1)],
	receive finish_up -> 
		receive after 950 ->
			lists:map(fun(Temp) -> Temp ! terminate end, R),
			receiver(P, R)
		end
	end. 

receiver(P, R) -> receiver(P, R, []).
receiver(P, [], L) -> P ! {reply, lists:flatten(L)};
receiver(P, [_|Tail], L) -> 
	receive
		Values -> receiver(P, Tail, [Values|L])
	end.

reverse_hash(Fun, Inputs, P, ReverseImage) ->
	mark_inputs(Inputs),
	reverse_hash(Fun, Inputs, P, ReverseImage, []).

reverse_hash(Fun, Inputs, P, ReverseImage, L) -> 
	receive 
		terminate -> P ! L
		after 0 ->
			Hash = Fun(ReverseImage),
			case get(Hash) of
				undefined -> reverse_hash(Fun, Inputs, P, ReverseImage + 1, L);
				_ -> reverse_hash(Fun, Inputs, P, ReverseImage + 1, [{Hash, ReverseImage}|L])
			end
	end.

mark_inputs([]) -> done;
mark_inputs([Head|Tail]) -> 
	put(Head, true),
	mark_inputs(Tail).
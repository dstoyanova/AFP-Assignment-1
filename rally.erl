-module(rally).

-export([rally/3]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

% Main function 
rally(Acceleration, Braking, Track) -> 
	rally(Acceleration, Braking, calculate_speed(Acceleration, Braking, 0), 0, Track, calculate_track_letgth(Track, 0), 0, []).

rally(_, _, _, X, _, Units, Moves, _) when X > Units -> Moves;

rally(Acceleration, Braking, [], _, Track, Units, Moves, [{PUnits, PLimit}|Rest]) ->
	rally(Acceleration, Braking, PLimit, PUnits, Track, Units, Moves - 1, Rest);

rally(Acceleration, Braking, [Speed|Tail], X, Track, Units, Moves, Rest) ->
	V = Speed div 10,
	NewSpeed = calculate_speed(Acceleration, Braking, Speed),
	IsCorrect = is_correct_speed(V, Speed, move_through_track(X, Track)),
	if
		IsCorrect ->
			rally(Acceleration, Braking, NewSpeed, X + V, Track, Units, Moves + 1, [{X, Tail}|Rest]);
		true -> rally(Acceleration, Braking, Tail, X, Track, Units, Moves, Rest)
	end.

% Pass several units and compute the rest of the track 
move_through_track(X, [{Units, Limit}|Rest]) ->
	if
		X < Units ->
			[{Units - X, Limit}|Rest];
		X =:= Units ->
			Rest;
		true -> move_through_track(X - Units, Rest)
	end.

% Checks if the speed is correct in the current section 
is_correct_speed(_, _, [{0, 0}]) -> true;
is_correct_speed(X, Speed, [{Units, Limit}|Rest]) ->
	if
		X =< Units ->
			Speed =< Limit;
		Speed > Limit ->
			false;
		true -> is_correct_speed((X - Units), Speed, Rest)
	end.

% Calculates the possible speed and return them in a list 
calculate_speed(Acceleration, Braking, Speed) ->
	lists:reverse(calculate_speed_helper(Speed + Acceleration, max(Speed - Braking, 10), [])).

calculate_speed_helper(Max, Min, Rest) -> 
	if
		Max >= Min ->
			calculate_speed_helper (Max - 10, Min, [Max|Rest]);
		true -> Rest
	end.

% Calculates the total number of units on the track 
calculate_track_letgth([], Br) -> Br;
calculate_track_letgth([{Units, _}|Rest], Br) -> calculate_track_letgth(Rest, Br + Units).

%%% Unit Testing %%%
rally_test_() -> [?_assertEqual(R, rally(lists:nth(1, N), lists:nth(2, N), lists:nth(3, N))) || {N,R} <- [{[30, 10, [{10,100},{5,70},{3,40},{6,100},{0,0}]], 5}, 
       												       {[40, 50, [{15,100},{0,0}]], 3}, 
       												       {[40, 20, [{1,50},{1,40},{1,30},{1,20},{1,10},{1,20},{1,30},{1,40},{1,50},{0,0}]], 5}]]. 
-module(vector_logic).

-export([vector/1]).
-export([vector_add/2, vector_sub/2, vector_dot/2]).
-export([vector_mul/2, vector_div/2, norm_one/1, norm_inf/1]).
-export([is_valid_length/1, is_equal_length/2]).

vector(V) -> 
	case is_valid_length(V) of
		true -> V;
		false -> error
	end.

vector_add(A, B) ->
	case is_equal_length(A, B) and is_valid_length(A) and is_valid_length(B) of
		true -> lists:zipwith(fun(X, Y) -> X + Y end, A, B);
		false -> error
	end.

vector_sub(A, B) ->
	case is_equal_length(A, B) and is_valid_length(A) and is_valid_length(B) of
		true -> lists:zipwith(fun(X, Y) -> X - Y end, A, B);
		false -> error
	end.

vector_dot(A, B) ->
	case is_equal_length(A, B) and is_valid_length(A) and is_valid_length(B) of
		true -> lists:zipwith(fun(X, Y) -> X * Y end, A, B);
		false -> error
	end.

vector_mul(Number, V) -> 
	case is_valid_length(V) of
		true -> [X * Number || X <- V];
		false -> error
	end.

vector_div(0, _) -> error;
vector_div(Number, V) -> 
	case is_valid_length(V) of
		true -> [X div Number || X <- V];
		false -> error
	end.

norm_one(V) -> 
	case is_valid_length(V) of
		true -> lists:foldl(fun(X, Sum) -> abs(X) + Sum end, 0, V);
		false -> error
	end.

norm_inf(V) -> 
	case is_valid_length(V) of
		true -> lists:max([abs(X) || X <- V]);
		false -> error
	end.

is_valid_length(V) -> 
	length(V) < 101.

is_equal_length(A, B) -> 
	length(A) =:= length(B). 
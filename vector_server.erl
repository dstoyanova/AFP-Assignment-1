%%%-------------------------------------------------------------------
%%% @author Desislava Stoyanova 
%%% @e-mail dess.stoyanova@icloud.com 
%%% @copyright 2015 Desislava Stoyanova 
%%% @doc This module defines a server process that listens for incoming
%%%      TCP connections and allows the user to execute commands via
%%%      that TCP stream.
%%% @end
%%%-------------------------------------------------------------------
-module(vector_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

% API
-export([start_link/1, start_link/0, get_count/0, stop/0]).

% gen_server behaviour 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([calculate/1]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

start_link() ->
	start_link(?DEFAULT_PORT).

get_count() ->
	gen_server:call(?SERVER, get_count).

stop() ->
	gen_server:cast(?SERVER, stop).

% vector_mul(Number, L) -> 
% 	gen_server:call(?SERVER, {vector_mul, Number, L}).

% vector_div(Number, L) -> 
% 	gen_server:call(?SERVER, {vector_div, Number, L}).

% norm_one(L) ->
% 	gen_server:call(?SERVER, {norm_one, L}).

% norm_inf(L) -> 
% 	gen_server:call(?SERVER, {norm_inf, L}).

init([Port]) ->
	{ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
	{ok, #state{port = Port, lsock = LSock}, 0}.

% handle_call({vector_mul, Number, L}, _From, State) ->
% 	{reply, vector_logic:vector_mul(Number, L), State}.

% handle_call({vector_div, Number, L}, _From, State) ->
% 	{reply, vector_logic:vector_div(Number, L), State}.

% handle_call({norm_one, L}, _From, State) ->
% 	{reply, vector_logic:norm_one(L), State}.

% handle_call({norm_inf, L}, _From, State) ->
% 	{reply, vector_logic:norm_inf(L), State}.

% handle_call(get_count, _From, State) ->
% 	{reply, {ok, State#state.request_count}, State}.

handle_call(_Request, _From, State) ->
	{reply, {ok, State#state.request_count}, State}.
	% case _Request of
	% 	get_count ->
	% 		{reply, {ok, State#state.request_count}, State};
	% 	{vector_mul, Number, L} -> 
	% 		{reply, vector_logic:vector_mul(Number, L), State};
	% 	{vector_div, Number, L} ->
	% 		{reply, vector_logic:vector_div(Number, L), State};
	% 	{norm_one, L} ->
	% 		{reply, vector_logic:norm_one(L), State};
	% 	{norm_inf, L} -> 
	% 		{reply, vector_logic:norm_inf(L), State}
	% end.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
	RequestCount = State#state.request_count,
	do_rpc(Socket, RawData),
	{noreply, State#state{request_count = RequestCount + 1}};

handle_info(timeout, #state{lsock = LSock} = State) ->
	{ok, _Sock} = gen_tcp:accept(LSock),
	{noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

do_rpc(Socket, RawData) ->
	% try
	% 	{M, F, A} = split_out_mfa(RawData),
	% 	Result = apply(M, F, A),
	% 	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
	% catch
	% 	_Class:Err ->
	% 	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
	% end.
	try
		MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
		[H|_] = args_to_terms(MFA),
		gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [calculate(H)]))
	catch
		_C:E ->
			gen_tcp:send(Socket, io_lib:fwrite("~p~n", [E]))
	end.

% split_out_mfa(RawData) ->
%     MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
%     {match, [M, F, A]} =
%         re:run(MFA,
%                "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
%                    [{capture, [1,2,3], list}, ungreedy]),
%     {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms([]) -> [];
args_to_terms(RawArgs) ->
	{ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
	{ok, Args} = erl_parse:parse_term(Toks),
	Args.

calculate(Arg) -> calculate(100, Arg).
calculate(_, error) -> error;
calculate(0, _) -> error;
calculate(_, []) -> error;
calculate(_, [H|T]) -> vector_logic:vector([H|T]);
calculate(N, {'add', A, B}) -> vector_logic:vector_add(calculate(N-1, A), calculate(N-1, B));
calculate(N, {'sub', A, B}) -> vector_logic:vector_sub(calculate(N-1, A), calculate(N-1, B));
calculate(N, {'dot', A, B}) -> vector_logic:vector_dot(calculate(N-1, A), calculate(N-1, B));
calculate(N, {'mul', Number, A}) -> vector_logic:vector_mul(calculate(N-1, Number), calculate(N-1, A));
calculate(N, {'div', Number, A}) -> vector_logic:vector_div(calculate(N-1, Number), calculate(N-1, A));
calculate(N, {'norm_one', A}) -> vector_logic:norm_one(calculate(N-1, A));
calculate(N, {'norm_inf', A}) -> vector_logic:norm_inf(calculate(N-1, A));
calculate(_, Number) -> Number. 

start_test() -> {ok, _} = tr_server:start_link(1055). 
%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to twitter_admin
-module(twitter_admin).

%%
%% Include files
%%
-include("twitter_admin.hrl").

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start/1,
		 stop/0,
		 stop/1
		 ]).

-export([
		 loop/0
		 ]).

%%
%% API Functions
%%
start() -> run(check).

start(ping) -> run(ping);

start(["ping"]) ->	run(ping);

start([ping]) ->   run(ping);

start(stop) ->	 run(stop);

start([stop]) -> run(stop);

start(["stop"]) ->	run(stop);

start(Other) ->
	io:format("invalid command [~p]~n", [Other]).

stop() ->
	?MODULE ! {stop, ok}.

stop(Code) ->
	?MODULE ! {stop, Code}.

%%
%% Local Functions
%%
run(Command) ->
	
	%% Register ourselves as message switch
	Pid = spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	
	?MODULE ! {run, Command},
	{ok, Pid}.


loop() ->
	receive
		{stop, ok} ->
			exit(ok);
		
		{stop, Code} ->
			halt(Code);
		
		{run, Command} ->
			handleCommand(Command)
	
	after 3000 ->
			
			halt(?DAEMON_NOT_RESPONDING)
		
	end,
	loop().



handleCommand(undefined) ->
	?MODULE ! {stop, undefined_command};

handleCommand(stop) ->
	Node=tools:make_node(twitter),
	io:format("twitteradmin: node: ~p~n",[Node]),
	case rpc:call(Node, twitter_app, stop, [], 2000) of
		{badrpc, Reason} ->
			io:format("daemon communication error [~p]~n", [Reason]),
			stop(?DAEMON_COMM_ERROR);
		
		{pong, _Pid} ->
			io:format("daemon found~n"),
			stop(?DAEMON_FOUND);
	
		Other ->
			io:format("twitteradmin: received [~p]~n", [Other])
	end;
	

handleCommand(ping) ->
	Running=tools:is_running(twitter),
	case Running of
		true ->
			io:format("daemon not found~n"),
			stop(?DAEMON_NOT_FOUND);
	
		%% Try pinging the daemon
		false ->
			do_ping()
	end.


%% Test if daemon is running locally
%%
%% @TODO make this more robust
do_ping() ->
	Node=tools:make_node(twitter),
	io:format("twitteradmin: node: ~p~n",[Node]),
	case rpc:call(Node, twitter_app, ping, [], 2000) of
		{badrpc, Reason} ->
			io:format("daemon communication error [~p]~n", [Reason]),
			stop(?DAEMON_COMM_ERROR);
		
		{pong, _Pid} ->
			io:format("daemon found~n"),
			stop(?DAEMON_FOUND);
	
		Other ->
			io:format("twitteradmin: received [~p]~n", [Other])
	end.



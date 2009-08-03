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
start() ->
	io:format("usage: twitter_admin [check]~n").

start(["check"]) ->
	run(check);

start([check]) ->
	run(check);

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
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	
	?MODULE ! {run, Command},
	{ok, Pid}.


loop() ->
	receive
		{stop, ok} ->
			exit(ok);
		
		{stop, Code} ->
			exit(Code);
		
		{run, Command} ->
			handleCommand(Command)
	
	after 3000 ->
			
			halt(?DAEMON_NOT_RESPONDING)
		
	end,
	loop().



handleCommand(undefined) ->
	?MODULE ! {stop, undefined_command};


handleCommand(check) ->
	Running=tools:is_running(),
	case Running of
		true ->
			io:format("daemon not found~n"),
			stop(?DAEMON_NOT_FOUND);
	
		%% Try pinging the daemon
		false ->
			do_ping()
	end.


%% TODO make this more robust
do_ping() ->
	Host=tools:extract_host(node()),
	Twitter=string:concat("twitter@", Host),
	AT=erlang:list_to_atom(Twitter),
	%%io:format("node: ~p~n",[AT]),
	case rpc:call(AT, twitter, ping, [], 2000) of
		{badrpc, Reason} ->
			io:format("daemon communication error [~p]~n", [Reason]),
			stop(?DAEMON_COMM_ERROR);
		
		{pong, _Pid} ->
			io:format("daemon found~n"),
			stop(?DAEMON_FOUND);
	
		Other ->
			io:format("twitteradmin: received [~p]~n", [Other])
	end.



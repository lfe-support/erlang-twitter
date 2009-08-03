%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to twitter_admin
-module(twitter_admin).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start/1,
		 stop/0
		 ]).

-export([
		 loop/0
		 ]).

%%
%% API Functions
%%
start() ->
	io:format("usage: [start|stop]~n").

start([start]) ->
	run(start);

start([stop]) ->
	run(stop);

start(Other) ->
	io:format("invalid command [~p]~n", [Other]).

stop() ->
	?MODULE ! {stop, ok}.

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
		{stop, Code} ->
			exit(Code);
		
		{run, Command} ->
			handleCommand(Command)
		
	end,
	loop().



handleCommand(undefined) ->
	?MODULE ! {stop, undefined_command};


handleCommand(start) ->
	ok;

handleCommand(stop) ->
	ok.


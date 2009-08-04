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

start(dostop) ->	 run(dostop);

start([dostop]) -> run(dostop);

start(["dostop"]) ->	run(dostop);

start(Command) -> run(Command);

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

handleCommand(dostop) ->
	case dorpc(dostop) of
		
		rpcerror ->
			stop(?DAEMON_COMM_ERROR);
		
		Other ->
			io:format("twitteradmin: received [~p]~n", [Other])
	end;
	

handleCommand(ping) ->
	case dorpc(ping) of 
		rpcerror ->
			stop(?DAEMON_COMM_ERROR);
	
		{pong, Pid} ->
			io:format("daemon found, pid[~p]~n", [Pid]),
			stop(?DAEMON_FOUND);
	
		Other ->
			io:format("communication protocol error [~p]~n", [Other]),
			stop(?DAEMON_PROTOCOL_ERROR)
	
	end;



handleCommand(pong) ->
	case dorpc(pong) of 
		rpcerror ->
			stop(?DAEMON_COMM_ERROR);
	
		{ping, Pid} ->
			io:format("daemon found, pid[~p]~n", [Pid]),
			stop(?DAEMON_FOUND);
	
		Other ->
			io:format("communication protocol error [~p]~n", [Other]),
			stop(?DAEMON_PROTOCOL_ERROR)
	end.


dorpc(Message) ->
	Node=tools:make_node(twitter),

	case rpc:call(Node, twitter_app, Message, [], 4000) of
		{badrpc, Reason} ->
			io:format("daemon communication error [~p]~n", [Reason]),
			rpcerror;
		
		Other ->
			io:format("twitteradmin:dorpc: received [~p]~n", [Other]),
			Other
	end.
	

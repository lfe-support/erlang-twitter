%% Author: Jean-Lou Dupont
%% Created: 2009-09-04
%% Description: TODO: Add description to twitter_app
-module(twitter_app).


%%
%% API functions
%%
-export([
		 start/0,
		 stop/0,
		
		 %%keep compiler happy
		 loop/0
		 ]).

start() ->
	Pid=spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	Pid ! start,
	{ok, Pid}.

stop() ->
	?MODULE ! stop.


%% Just to keep things running
loop() ->
	receive
		start ->
			inets:start(),
			twitter_sup:start_link();
		
		stop ->
			exit(normal)
	end,
	loop().


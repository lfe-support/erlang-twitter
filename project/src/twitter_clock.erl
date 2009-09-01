%% Author: Jean-Lou Dupont
%% Created: 2009-08-30
%% Description: Clock and Sync
%%
%% @doc
%% =Clock=
%% This module generates the 'system clock' and 'system sync' messages.
%%
%% The system clock is 1minute based.
%%
%% The system sync is defined by list based pattern: each entry corresponds
%% to a millisecond-based delay at which expiration a message 'clock.tick.sync'
%% is generated.
%%
%% The 'clock.tick.sync' helps the application to converge to a stable configration quicker.
%%
%% =Duties=
%% <ul>
%%  <li>Generate 'clock.tick.min'</li>
%%  <li>Generate 'clock.tick.sync'</li>
%% </ul>
%%
-module(twitter_clock).

-define(SERVER, clock).
-define(BUSSES, []).
-define(SWITCH, twitter_hwswitch).
-define(MINUTE, 60*1000).

-define(SYNC_PATTERN, [100, 250, 500, 750, 1000, 2000, 3000]).


%%
%% Exported Functions
%%
-export([
		 start_link/0
		,stop/0
		,get_server/0
		,get_busses/0
		 ]).

%%
%% LOCAL functions
%%
-export([
		 loop/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).


%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  Management  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.

start_link()->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! start,
	{ok, Pid}.

stop() ->
	try 
		?SERVER ! stop,
		ok
	catch
		_:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		%%% MANAGEMENT RELATED %%%
		start->
			put(sync_pattern, ?SYNC_PATTERN),
			do_sync();
		
		stop ->
			exit(normal);
		
		{sync, Delay} ->
			?SWITCH:publish(clock, {tick.sync, Delay}),
			do_sync();
		
		Other ->
			log(warning, "clock: unexpected message: ", [Other])
	
	after ?MINUTE ->
			
		Count=get_and_inc_count(),
		?SWITCH:publish(clock, {tick.min, Count})
	
	end,
	loop().



get_and_inc_count() ->
	Count=get(count),
	get_and_inc_count(Count).
	
get_and_inc_count(undefined) ->
	put(count, 1),
	1;

get_and_inc_count(Count) ->
	put(count, Count+1),
	Count.



do_sync() ->
	Pattern=get(sync_pattern),
	do_sync(Pattern).

do_sync([]) ->
	finished;

do_sync([Delay|Tail]) ->
	timer:send_after(Delay, {sync, Delay}),
	put(sync_pattern, Tail).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  CONFIG  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc List of parameters which cannot be customized
%%
%% @spec blacklist() -> list()
%%
blacklist() ->
	[].

%% @doc List of default parameters for the module
%%
%% @spec defaults() -> list()
%%
defaults() ->
	[].


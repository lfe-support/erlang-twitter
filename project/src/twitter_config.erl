%% Author: Jean-Lou Dupont
%% Created: 2009-08-28
%% Description: Configuration Agent
%%
%% @doc 
%% = Configuration Agent =
%%
%% == Duties ==
%% <ul>
%%  <li>Listen for 'reload' on sys bus</li>
%%  <li>Listen for 'announce.mod' on sys bus</li>
%%  <li>Generate 'config.mod' on sys bus</li>
%% </ul>
%%
%% == Local Switch Busses ==
%%
%% <ul>
%%  <li>sys: reload, announce.mod</li>
%%  <li></li>
%% </ul>
%%
%%
%% <ul>
%%  <li></li>
%% </ul>

-module(twitter_config).

-define(SWITCH, twitter_hwswitch).
-define(SERVER, config).

%%
%% Management Functions
%%
-export([
		 start_link/0
		 ,stop/0
		,loop/0
		 ]).


%%
%% API functions
%%
-export([
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
start_link() ->
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
		start ->
			ok;
		
		stop ->
			exit(normal);
	
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "Unexpected message: ", [Other])
	end,
	loop().

%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%% Not much to do
handle({hwswitch, _From, sys, reload}) ->
	ok;


handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).







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




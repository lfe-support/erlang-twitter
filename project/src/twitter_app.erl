%% Author: Jean-Lou Dupont
%% Created: 2009-08-30
%% Description: App module
%%
%% = App Agent =
%%
%% This module's primary function is to correlate the modules configuration version
%% with that advertised through the message 'sys.config'.
%% Each module advertises its current configuration version and a configuration master
%% (usually the module Config) sends out 'sys.config' message indicating the currently
%% in-force configuration version.  When all modules advertise their compliance to
%% the in-force configuration, this module generates:
%% ```
%%  {app, sys, app.ready}
%% '''
%%
%% =Duties=
%% <ul>
%%  <li>Listens for 'clock.tick.min'  and generates 'sys.app.modules'</li>
%%  <li>Listens for 'clock.tick.sync' and generates 'sys.app.modules'</li>
%%  <li>Listens for 'sys.mod.config'</li>
%%  <li>Listens for 'sys.config.</li>
%%  <li>Generates 'sys.app.ready'</li>
%% </ul>
%%
%%
%% <ul>
%%  <li></li>
%% </ul>

-module(twitter_app).

-define(SERVER, app).
-define(SWITCH, twitter_hwswitch).
-define(BUSSES, [sys, clock]).

%%
%% API Functions
%%
-export([
		 start_link/1
		,stop/0
		]).

%%
%% LOCAL Functions
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
start_link(Modules) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {start, Modules},
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
		{start, Modules} ->
			put(modules, Modules);
		
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


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Version}}) ->
	ok;

%% The in-force configuration version is announced
handle({hwswitch, _From, sys, {config, Version}}) ->
	ok;

handle({hwswitch, _From, clock, {tick.min, Count}}) ->
	ok;

handle({hwswitch, _From, clock, {tick.sync, Count}}) ->
	ok;


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


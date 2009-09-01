%% Author: Jean-Lou Dupont
%% Created: 2009-08-30
%% Description: App module
%%
%% = App Agent =
%%
%% This module's primary function is to correlate the modules configuration version
%% with that advertised through the message 'sys.config'.
%%
%% Each module advertises its current configuration version and a configuration master
%% (usually the module Config) sends out 'sys.config' message indicating the currently
%% in-force configuration version.  
%%
%% When all modules advertise their compliance to the in-force configuration, 
%% this module generates:
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
		,get_server/0
		,get_busses/0
		]).

%%
%% LOCAL Functions
%%
-export([
		 loop/0
		,check/0
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


start_link(Modules) when is_list(Modules)->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! {start, Modules},
	{ok, Pid};

start_link(_) ->
	{error, invalid_param}.

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
			put(modules.count, length(Modules)),
			put(modules, Modules);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "app: unexpected message: ", [Other])
	end,
	loop().

%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%% A module advertises its configuration version
handle({hwswitch, From, sys, {mod.config, Version}}) ->
	put({mod.version, From}, Version),
	check();

%% The in-force configuration version is announced
handle({hwswitch, _From, sys, {config, Version}}) ->
	put(config.version, Version),
	check();
	
handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	check();

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	check();

handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc Performs a correlation check
%%
check() ->
	Version=get(config.version),
	Modules=get(modules),
	check(Modules, Version, 1).

check([], _, _) ->
	end_list;

check(_, undefined, _) ->
	no_version_in_force;

check([Module|Rest], Version, Count) ->
	ModVersion=get({mod.version, Module}),
	check_one(Version, ModVersion, Count),
	check(Rest, Version, Count+1).


check_one(Version, ModVersion, Count) ->
	
	%% Version match?
	case Version==ModVersion of
		true ->
			maybe_generate_ready(Count);
		false ->
			ok
	end.

maybe_generate_ready(Count) ->
	TotalCount=get(modules.count),
	maybe_generate_ready(Count, TotalCount).

%% cycled through all modules and versions match?
maybe_generate_ready(X, X) ->
	?SWITCH:publish(sys, app.ready);

maybe_generate_ready(_, _) ->
	version_no_match.



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


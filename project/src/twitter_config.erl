%% Author: Jean-Lou Dupont
%% Created: 2009-08-28
%% Description: Configuration Agent
%%
%% @doc 
%% = Configuration Agent =
%%
%% This module listens to 'sys.reload' and reloads configuration information from file.
%% Once the new configuration is parsed and validated, this module generates the
%% message 'sys.config' advertising the in-force Version# of the configuration 
%% for the application.
%%
%% This module listens for 'clock.tick.min' and 'clock.tick.sync' messages
%% and generates 'sys.config' upon reception.
%%
%% This module listens for 'sys.mod.config' message and sends to the Source of this
%% message its configuration information. E.g. 
%% ```
%%  {policer, sys, {mod.config, Version}}
%% '''
%% if the parameter 'Version' does not correspond to the latest configuration version,
%% this module sends to the source (and this case 'policer') its configuration information
%% using the message format:
%% ```
%%  {config, Version, Configuration}
%% '''
%%
%% == Duties ==
%% <ul>
%%  <li>Listen for 'sys.reload'</li>
%%  <li>Listen for 'sys.mod.config'</li>
%%  <li>Listen for 'clock.tick.min'</li>
%%  <li>Listen for 'clock.tick.sync'</li>
%%  <li>Generate 'sys.config' (i.e. configuration version)</li>
%% </ul>
%%
%%
%% <ul>
%%  <li></li>
%% </ul>

-module(twitter_config).

-define(BUSSES, [sys, clock]).
-define(SWITCH, twitter_hwswitch).
-define(SERVER, config).
-define(TOOLS, twitter_tools).
-define(CTOOLS, twitter_ctools).

%%
%% Management Functions
%%
-export([
		 start_link/1
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

start_link(_Args) ->
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


handle({hwswitch, _From, sys, reload}) ->
	do_load_config();

% A module announces its configuration version
handle({hwswitch, From, sys, {mod.config, Module, ConfigVersion}}) ->
	maybe_send_config_to_module(From, Module, ConfigVersion);

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	'sys.config'();

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	'sys.config'();	

handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  DOERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

do_load_config() ->
	
	%% erase all process information
	erase(),
	
	Result=?CTOOLS:do_config(),
	case Result of
		{ok, Version, Config} ->
			put(config, Config),
			put(version, Version);
		Other ->
			log(error, "error processing configuration file. Reason: ", [Other])			
	end.


'sys.config'() ->
	Version=get(version),
	?SWITCH:publish(sys, {config, Version}).


maybe_send_config_to_module(ModuleServer, Module, ModuleConfigVersion) ->
	CurrentVersion=get(version),
	send(ModuleServer, Module, ModuleConfigVersion, CurrentVersion).

% can't do much right now...
send(_ModuleServer, _Module, _ModuleConfigVersion, undefined) ->
	ok;

send(_ModuleServer, Module, X, X) ->
	log(debug, "Configuration up-to-date for module: ",[Module]);


send(ModuleServer, Module, _ModuleConfigVersion, CurrentVersion) ->
	Config=get(config),
	maybe_send(ModuleServer, Module, Config, CurrentVersion).
	

maybe_send(_Server, _Module, undefined, _CurrentVersion) ->
	log(debug, "configuration not available");

maybe_send(Server, Module, Config, CurrentVersion) ->
	ModuleConfig=?TOOLS:kfind(Module, Config),
	case ModuleConfig of
		{_, ConfigEntries} ->
			safe_send(Server, {config, CurrentVersion, ConfigEntries});
		Other ->
			log(error, "error retrieving config info, {Module, Reason}", [Module, Other])
	end.

	

safe_send(Server, Msg) ->
	try
		Server ! Msg
	catch
		_:_ ->
			log(error, "error sending configuration to module server:", [Server])
	end.

			

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

log(Severity, Msg) ->
	log(Severity, Msg, []).

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



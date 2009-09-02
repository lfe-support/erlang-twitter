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
-define(CTOOLS, twitter_ctools).

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
		
		{config, Version, Config} ->
			put(config.version, Version),
			?CTOOLS:put_config(Config);
		
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

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	check(),
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	check(),
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	put(config.version.inforce, VersionInForce);


%% A module advertises its configuration version
handle({hwswitch, _From, sys, {mod.config, Module, Version}}) ->
	put({mod.version, Module}, Version),
	check();

handle({hwswitch, _From, sys, _Msg}) ->
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;



handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

make_list(List) when is_list(List) -> List;
make_list(Term) -> [Term].


%% @doc Performs a correlation check
%%
check() ->
	VersionInForce=get(config.version.inforce),
	Modules=make_list(get(modules))--[?MODULE],
	Count=length(Modules),
	do_check(Modules, VersionInForce, Count, 0).

do_check(_, undefined, _, _) ->
	config_not_started;

do_check(_, _VersionInForce, X, X) ->
	?SWITCH:publish(sys, app.ready);

do_check([], _, _, _Count) ->
	%io:format("APP NOT READY, count[~p]~n", Count),
	not_ready;

do_check([Module|Modules], VersionInForce, ModulesCount, CurrentCount) ->
	ModuleServer=?CTOOLS:get_module_server(Module),
	ModVersion=get({mod.version, ModuleServer}),
	io:format("do_check: mod[~p] vif[~p] mv[~p] mc[~p] cc[~p]~n", [Module, VersionInForce, ModVersion, ModulesCount, CurrentCount]),
	case VersionInForce==ModVersion of
		true  -> NewCount=CurrentCount+1;
		false -> NewCount=CurrentCount
	end,
	do_check(Modules, VersionInForce, ModulesCount, NewCount).




%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
set_state(State) ->
	put(state, State).

%get_state() ->
%	State=get(state),
%	case State of
%		undefined -> working;   %start in 'working' state
%		working   -> working;
%		_         -> suspended
%	end.


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


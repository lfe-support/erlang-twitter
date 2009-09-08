%% Author: Jean-Lou Dupont
%% Created: 2009-09-04
%% Description: 
%%
%% @doc
%%
-module(template).

-define(SERVER, updater).
-define(BUSSES, [sys, clock, notif, tweet]).
-define(SWITCH, twitter_hwswitch).
-define(CTOOLS, twitter_ctools).


%%
%% API functions
%%
-export([
		 start_link/0
		,get_server/0
		,get_busses/0
		,stop/0
		 ]).

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

start_link() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! start,
	{ok, Pid}.

stop() ->
	try   ?SERVER ! stop, ok
	catch _:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		stop ->
			exit(normal);
	
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "updater: unexpected message: ", [Other])
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%%%%%%%%%%%%%% SYS bus
%---------------------

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, sys, suspend}) ->	set_state(suspended);
handle({hwswitch, _From, sys, resume}) ->	set_state(working);

handle({hwswitch, _From, sys, _}) ->
	not_supported;


%%%%%%%%%%%%%% CLOCK bus
%-----------------------

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;


%%%%%%%%%%%%%% CATCH-ALL
%-----------------------

handle(Other) ->
	log(warning, "snooper: unexpected message: ", [Other]).



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
set_state(State) ->
	put(state, State).

get_state() ->
	State=get(state),
	case State of
		undefined -> working;   %start in 'working' state
		working   -> working;
		_         -> suspended
	end.


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
	[
	 %% bypass active by default currently... no policer defined yet.
	 {updater.bypass, optional, atom, true}
	
	 %% Echo the reply following 'statuses.update' API
	,{updater.echo,   optional, atom, true}
	 ].


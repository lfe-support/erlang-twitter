%% Author: Jean-Lou Dupont
%% Created: 2009-09-03
%% Description: TODO: Add description to twitter_logpolicer
-module(twitter_logpolicer).

-define(SERVER, logpolicer).
-define(SWITCH, twitter_hwswitch).
-define(BUSSES, [sys, clock, log]).
-define(CTOOLS, twitter_ctools).
-define(TOOLS,  twitter_tools).
-define(LOG,    twitter_log).

%%
%% API Exported Functions
%%
-export([
		 start_link/0
		 ,stop/0
		 ,get_server/0, get_busses/0
		
		 %% keep compilation warning free
		,loop/0
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
	{ok, Pid}.


stop() ->
	try ?SERVER ! stop, ok
	catch _:_ -> {error, cannot_stop}
	end.


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
			
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "logpolicer: unexpected message: ", [Other])
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;



handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, sys, _Msg}) ->
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;


handle({hwswitch, _From, log, {Context, {Severity, Msg, Params}}}) ->
	filter(Context, Severity, Msg, Params);
	

handle(Other) ->
	log(warning, "status: Unexpected message: ", [Other]).



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  FILTERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

filter(app.ready, Severity, Msg, Params) ->
	'log.once.per.run'(Severity, Msg, Params);

filter(_Context, Severity, Msg, Params) ->
	maybe_dolog(Severity, Msg, Params).


'log.once.per.run'(Sev, Msg, Ps) ->
	CurrentPid=os:getpid(),
	SavedPid=get(pid),
	case CurrentPid==SavedPid of
		true  -> 'dont.log.again';
		false ->
			put(pid, CurrentPid),
			maybe_dolog(Sev, Msg, Ps)
	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


maybe_dolog(Severity, Msg, Params) ->
	FlagVarName=?TOOLS:make_atom_from_list([logpolicer,'.',Severity]),
	Flag=get(FlagVarName),
	%io:format("maybe_dolog: fn [~p] f[~p] s[~p] m[~p]~n", [FlagVarName, Flag, Severity, Msg]),
	maybe_dolog(Flag, Severity, Msg, Params).


maybe_dolog(false, _Severity, _Msg, _Params) -> blocked;
maybe_dolog(true, Severity, Msg, Params) ->
	log(Severity, Msg, Params);
maybe_dolog(undefined, Severity, Msg, Params) ->
	log(Severity, Msg, Params).




log(Severity, Msg) ->
	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?LOG:policed_log(Severity, Msg, Params).



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
	   {logpolicer.debug,    optional, atom, false}	 
	  ,{logpolicer.critical, optional, atom, true}
	  ,{logpolicer.info,     optional, atom, true}	
	 ].


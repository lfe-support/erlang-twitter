%% Author: Jean-Lou Dupont
%% Created: 2009-09-02
%% Description: 
%%
%% @doc 
%%
%% ==Twitter Status==
%%
%% <ul>
%%  <li>Listen on 'tweet' bus for 'tweet.account' messages</li>
%%  <li>Send status updates using 'tweet.status' messages</li>
%%  <li></li>
%% </ul>
-module(twitter_status).

-define(SERVER, status).
-define(SWITCH, twitter_hwswitch).
-define(BUSSES, [sys, clock, tweet]).
-define(CTOOLS, twitter_ctools).

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
			io:format("status:configdata: ~p~n", [Config]),
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
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


handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);


handle({hwswitch, _From, sys, _Msg}) ->
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;




handle(Other) ->
	log(warning, "status: Unexpected message: ", [Other]).


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
	[
	  {status.poll,     optional, int, 5*60*1000}  %% in milliseconds
	 ,{status.poll.min, optional, int, 1*60*1000}  %% in milliseconds
	 ,{status.poll.max, optional, int, 15*60*1000} %% in milliseconds
	 ].


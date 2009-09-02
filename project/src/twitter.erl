%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter API
%%
%% Support for additional methods can be easily added.
%%
%% Use twitter_tools:extract to retrieve a specific field
%%  of the result-set returned by twitter_api:request
%%
%% Daemon:
%%  - reads configuration file in ~/.twitter
%%  - can reload the configuration file
%%  - status of mswitch subscription
%%  - 

-module(twitter).

%%
%% Macros 
%%
-define(DEFAULTS, twitter_defaults).
-define(POLICER,  twitter_policer).
-define(SERVER,   twitter).
-define(TOOLS,    twitter_tools).
-define(CTOOLS,   twitter_ctools).
-define(REQ,      twitter_req).
-define(TAPI,     twitter_api).
-define(MNG,      twitter_mng).
-define(RPC,      twitter_rpc).

-define(SWITCH,      twitter_hwswitch).
-define(BUSSES,      [sys, clock, notif]).
-define(TIMER_CHECK, 60*1000).

%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 stop/0,
		 daemon_api/2
		,get_server/0
		,get_busses/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).


-define(TIMEOUT, 5000).

%% LOCAL
-export([
		 loop/0
		 ]).


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------
get_server() -> ?SERVER.
get_busses() -> ?BUSSES.


%% Start Link
%%
%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	{ok,Pid}.

%% Stop
%%
%% @spec stop() -> ok | {error, Reason} 
stop() ->
	try   ?SERVER ! stop, ok
	catch _:_ -> {error, cannot_stop}
	end.


%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------


%% @private
loop() ->
	receive
		
		stop ->
			exit(normal);
		
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);

		
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
		
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);

		
		{request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams} ->
			?TAPI:request(ReplyDetails, ?TIMEOUT, Auth, Method, MandatoryParams, OptionalParams);

		{request, ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams} ->
			?TAPI:request(ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams);

		{http, {RequestId, {error, Reason}}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			?REQ:reply(ReturnDetails, {error, Reason});

		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			?REQ:reply(ReturnDetails, {response, Result})
		
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

handle({hwswitch, _From, sys, suspend}) ->
	put(state, suspended);

handle({hwswitch, _From, sys, resume}) ->
	put(state, working);

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).



%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------


daemon_api(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			?MNG:inc_stat(error_daemon_api_invalid_command),
			{error, invalid_command}
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
	 {twitter.user,     mandatory, nstring, ""}
	,{twitter.pass,     mandatory, nstring, ""}
	,{twitter.simulate, optional,  atom,    false}
	 ].


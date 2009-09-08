%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter API
%%
%% Support for additional methods can be easily added.
%%
%% Use twitter_tools:extract to retrieve a specific field
%%  of the result-set returned by twitter_api:request
%%
%% @doc
%% == Operation ==
%%
%% 1) Upon 'app.ready' message reception, 
%%    parse config for 'twitter.account*' parameters
%%    and send on 'tweet' bus
%%
%% 
%%
-module(twitter).

%%
%% Macros 
%%
-define(SERVER,   twitter).
-define(TOOLS,    twitter_tools).
-define(CTOOLS,   twitter_ctools).
-define(REQ,      twitter_req).
-define(TAPI,     twitter_api).
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
		,test/0
		,reload/0
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
	%inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	%Pid ! start,
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
		
		start ->
			inets:start(httpc, []);
		
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

handle({hwswitch, _From, sys, app.ready}) ->
	Accounts=get_accounts(),
	?SWITCH:publish(tweet, {accounts, Accounts});
	%io:format("accounts: ~p~n",[Accounts]);


handle({hwswitch, _From, sys, _}) ->
	unsupported;


%% @TODO
handle({hwswitch, _From, notif, _}) ->
	unsupported;


handle(Other) ->
	log(warning, "twitter: Unexpected message: ", [Other]).



%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------


daemon_api(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
			%?MNG:inc_stat(error_daemon_api_invalid_command),
			{error, invalid_command}
	end.


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

%% @doc Go through the process dictionary and make a list
%%		from the variables 'twitter.account*'
get_accounts() ->
	do_get_accounts(1, []).

do_get_accounts(Count, Acc) ->
	Account=?TOOLS:make_atom_from_list([twitter,'.','account',Count]),
	{State, NewAcc}=try_one_var(Count, Account, Acc),
	case State of
		finished -> NewAcc;
		found    ->
			do_get_accounts(Count+1, NewAcc)
	end.

	
try_one_var(AccountId, Account, Acc) ->
	User=?TOOLS:make_atom_from_list([Account,'.user']),
	Pass=?TOOLS:make_atom_from_list([Account,'.pass']),
	%io:format("try: u[~p] p[~p]~n", [User, Pass]),
	UserVar=get(User),
	PassVar=get(Pass),
	maybe_add(AccountId, UserVar, PassVar, Acc).



maybe_add(_, _, undefined, Acc) ->
	{finished, Acc};

maybe_add(_, undefined, _, Acc) ->
	{finished, Acc};

maybe_add(AccountId, UserValue, PassValue, Acc) ->
	{found, Acc++[{AccountId, UserValue, PassValue}]}.	



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
	 %% prepare for possible 'load balancing'
	 {twitter.account1.user, mandatory, nstring,  ""}
	,{twitter.account1.pass, mandatory, nstring,  ""}
	 ].



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TEST  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

test() ->
	?SWITCH:publish(notif, {test, high, "test now: "++[now()] }).

reload() ->
	?SWITCH:publish(sys, reload).

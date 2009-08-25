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
-define(LOGGER,   twitter_log).
-define(LOG,      twitter_policed_logger).
-define(TOOLS,    twitter_tools).
-define(REQ,      twitter_req).
-define(TAPI,     twitter_api).
-define(MNG,      twitter_mng).
-define(RPC,      twitter_rpc).

%% MSWITCH busses to subscribe to
-define(BUSSES,        [notif]).
-define(MSWITCH_HEART, 30*1000).
-define(TIMER_CHECK,   60*1000).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start_link/0,
		 stop/0,
		 daemon_api/2
		 ]).


-define(TIMEOUT, 5000).

%% LOCAL
-export([
		 loop/0,
		 inbox/1
		 ]).


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%% Start
%%
%% @spec start() -> {ok, Pid}
start() ->
	inets:start(),
	Pid = spawn(?MODULE, loop, []),
	register(?SERVER, Pid),
	?SERVER ! reload,
	{ok,Pid}.

%% Start Link
%%
%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	?SERVER ! reload,
	{ok,Pid}.

%% Stop
%%
%% @spec stop() -> ok
stop() ->
	?SERVER ! stop,
	ok.


%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------


%% @private
loop() ->
	receive
		reload ->
			?LOGGER:init(),
			?MNG:load_config(),
			?POLICER:init(),
			?LOG:init(),
			
			config_timer(),
			do_sync();
		
		timer_tick ->
			do_heart();
		
		stop ->
			exit(ok);
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);

		%% Messages received 
		{mswitch, _From, Message} ->
			?MNG:inc_stat(mswitch_message_received),
			io:format("mswitch:msg: ~p~n", [Message]);
		
		
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


%% @doc Make sure that the timer is up & running
%%      & reload configuration
%%
config_timer() ->
	TimerRef=get(timer_ref),
	
	%% no exception is throwed if invalid
	timer:cancel(TimerRef),
	
	%% Get interval
	Interval=get({param, refresh_mswitch}),
	
	%% send 'timer_tick' message to self()
	Result=timer:send_interval(Interval, timer_tick),
	config_timer_handle_result(Result).

config_timer_handle_result({ok, Tref}) ->
	put(timer, Tref);

config_timer_handle_result(_) ->
	?MNG:inc_stat(error_timer_interval).
	


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


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MSWITCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

%% Performs synchronization with MSWITCH
%%
do_sync() ->
	try
		{_Pid, ok}=mswitch:subscribe({?MODULE, inbox, ?SERVER}, ?BUSSES),
		put(mswitch, found)
	catch
		error:undef ->
			?MNG:inc_stat(mswitch_not_found),
			put(mswitch, not_found),
			?LOG:log(mswitch_error, error, "mswitch not found"),
			error;

		_:_ ->
			?MNG:inc_stat(mswitch_node_not_found),
			put(mswitch, not_found),
			?LOG:log(mswitch_error, error, "mswitch node not found"),
			error
	end.

%% @doc Heartbeat publication on mswitch bus 'heart'
%%
do_heart() ->
	State=get(mswitch),
	case State of
		found ->
			%% we believe the mswitch is accessible
			do_heart_publish();
		_ ->
			%% we didn't find it the last time around...
			%% try this time
			do_sync(),
			do_heart_publish()
	end.

do_heart_publish() ->
	try
		{_Pid, ok}=mswitch:publish(heart, {twitter, now()}),
		put(mswitch, found)
	catch
		error:undef ->
			?MNG:inc_stat(mswitch_not_found),
			put(mswitch, not_found),
			error;

		_:_ ->
			?MNG:inc_stat(mswitch_node_not_found),
			put(mswitch, not_found),
			error
	end.
	


%% @doc MSWITCH Mailbox
%%
inbox({FromNode, Server, Message}) ->
	%%io:format("inbox: FromNode[~p] Server[~p] Message[~p]~n",[FromNode, Server, Message]),
	Server ! {mswitch, FromNode, Message}.


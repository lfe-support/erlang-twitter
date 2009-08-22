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
-define(SERVER, twitter).
-define(TOOLS,  twitter_tools).
-define(REQ,    twitter_req).
-define(TAPI,   twitter_api).
-define(MNG,    twitter_mng).
-define(RPC,    twitter_rpc).

%% MSWITCH busses to subscribe to
-define(BUSSES,        [notif]).
-define(MSWITCH_HEART, 30*1000).

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
	?SERVER ! start,
	{ok,Pid}.

%% Start Link
%%
%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	?SERVER ! start,
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
		start ->
			?MNG:load_config(),
			do_sync();
		
		stop ->
			exit(ok);
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, ReplyContext, Q}} ->
			?RPC:handle_rpc(ReplyTo, FromNode, ReplyContext, Q);

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
		
	after ?MSWITCH_HEART ->
			
			do_sync()
	
	end,
	loop().






%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------


daemon_api(ReplyContext, Command) ->
	case ?RPC:rpc_validate_command(Command) of
		true ->
			?RPC:rpc(ReplyContext, Command);
		_ ->
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
		ok
	catch
		error:undef ->
			?MNG:inc_stat(mswitch_not_found),
			error;

		_:_ ->
			?MNG:inc_stat(mswitch_node_not_found),
			error
	end.


%% Performs the MSWITCH subscriptions
%%
do_subscribe() ->
	ok.

do_heart() ->
	ok.



%% Mailbox
%%
inbox({FromNode, Server, Message}) ->
	%%io:format("inbox: FromNode[~p] Server[~p] Message[~p]~n",[FromNode, Server, Message]),
	Server ! {mswitch, FromNode, Message}.

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
%%

-module(twitter).

%%
%% Macros 
%%
-define(SERVER,twitter).
-define(TOOLS, twitter_tools).
-define(REQ,   twitter_req).
-define(TAPI,  twitter_api).
-define(MNG,   twitter_mng).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start_link/0,
		 stop/0,
		 req/5, req/6,
		 daemon_api/1
		 ]).


-define(TIMEOUT, 5000).

%% LOCAL
-export([
		 loop/0,
		 
		 %% Testing
		 test/0,
		 test_update/0,
		 test_bad/0
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
			?MNG:load_config();
		
		stop ->
			exit(ok);
		
		%% RPC bridge
		%%
		%% Messages ending up here are (usually ;-)
		%% sent through using the 'rpc' function.
		%%
		{rpc, ReplyTo, {FromNode, Q}} ->
			handle_rpc(ReplyTo, FromNode, Q);

		
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



%% ----------------------                     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REQUEST DISPATCHING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                     ------------------------------



%% @spec req(ReplyDetails, Auth, Method, MandatoryParams, OptionalParams) -> ok | error
%% where
%%		ReplyDetails = {From, Context}
%%			From = pid()
%%			Context = atom()
%%		Auth = {auth, Username, Password}
%%			Username = string()
%%			Password = string()
%%		Method = atom()
%%		MandatoryParams = [] | [{Key,Value}]
%%		OptionalParams  = [] | [{Key, Value}]
%% @doc
%% The parameter 'From'
%%
req(ReplyDetails, Auth, Method, MandatoryParams, OptionalParams) ->
	Ret = ?MODULE ! {request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams},
	case Ret of
		{request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams} ->
			ok;
		_ ->
			error
	end.

%% @spec req(ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams) -> ok | error
%% where
%%		ReplyDetails = {From, Context}
%%			From = pid()
%%			Context = atom()
%%		Timeout = integer()
%%		Auth = {auth, Username, Password}
%%			Username = string()
%%			Password = string()
%%		Method = atom()
%%		MandatoryParams = [] | [{Key,Value}]
%%		OptionalParams  = [] | [{Key, Value}]
req(ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams) ->
	Ret = ?MODULE ! {request, ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams},
	case Ret of
		{request, ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams} ->
			ok;
		_ ->
			error
	end.



%% ----------------------                   ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% DAEMON MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                   ------------------------------

%%
%% @spec daemon_api(status) -> {pid, Pid}
%%
%% @private
daemon_api(status) ->
	{pid, os:getpid()};

%% @spec daemon_api(reload) -> {error, Reason} | ok
daemon_api(reload) ->
	rpc(reload);

daemon_api(_) ->
	{error, invalid_command}.





%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% RPC handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------



handle_rpc(ReplyTo, _FromNode, reload) ->
	Result=?MNG:load_config(),
	rpc_reply(ReplyTo, Result);


handle_rpc(ReplyTo, _, _) ->
	rpc_reply(ReplyTo, {error, invalid_request}).


%% Replies to an RPC call.
%% The recipient loop of the message normally sits
%% in the code of the function 'rpc'.
%%
rpc_reply(ReplyTo, Message) ->
	ReplyTo ! {rpc_reply, Message}.

	
%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


%% @private
rpc(Q) ->
	FromNode=node(),
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%?TOOLS:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	?SERVER ! {rpc, self(), {FromNode, Q}},
	receive
		{rpc_reply, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			error_logger:error_msg("~p: rpc: timeout~n", [?MODULE]),
			{error, rpc_timeout}
	end.


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TESTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------


%% request(Rd, Auth, users.show, [{user_id, UserId}], []) ->
%% @private
%% @hidden
test() ->
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	%%Param=os:getenv("twitter_param"),
	req(undefined, {auth, Username, Password}, statuses.user_timeline, [], []).

%% @private
%% @hidden
test_update() ->
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	Param=os:getenv("twitter_param"),
	req(undefined, {auth, Username, Password}, statuses.update, [{status, Param}], []).

%% @private
%% @hidden
test_bad() ->
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	req(undefined, {auth, Username, Password}, bad.method, [], []).
	

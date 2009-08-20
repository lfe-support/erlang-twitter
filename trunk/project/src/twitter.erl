%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter API
%%
%% Support for additional methods can be easily added.
%%
%% Use twitter_tools:extract to retrieve a specific field
%%  of the result-set returned by twitter_api:request
%%

-module(twitter).

%%
%% Macros 
%%
-define(TOOLS, twitter_tools).
-define(REQ,   twitter_req).
-define(TAPI,  twitter_api).

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start_link/0,
		 stop/0,
		 req/5,
		 req/6
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
	register(?MODULE, Pid),
	{ok,Pid}.

%% Start Link
%%
%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok,Pid}.

%% Stop
%%
%% @spec stop() -> ok
stop() ->
	?MODULE ! stop,
	ok.


%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------


%% @private
loop() ->
	receive
		stop ->
			exit(ok);
		
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
	

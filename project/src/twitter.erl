%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter API
%%
%% Support for additional methods can be easily added.
%%
%% Use tools:extract to retrieve a specific field
%%  of the result-set returned by twitter_api:request
%%

-module(twitter).

%%
%% Macros 
%%
-define(TOOLS, twitter_tools).

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

-define(API,     "http://twitter.com/").
-define(TIMEOUT, 5000).

%% LOCAL
-export([
		 reply/2,
		request/6,
		 do_auth_request/4,
		 do_auth_request/5,
		 do_request/4,
		 do_request/5,
		 
		 loop/0,
		 
		 %% Testing
		 test/0,
		 test_update/0,
		 test_bad/0
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                      MODULE MANAGEMENT
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec start() -> {ok, Pid}
start() ->
	inets:start(),
	Pid = spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok,Pid}.

%% @spec start_link() -> {ok, Pid}
start_link() ->
	inets:start(),
	Pid = spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok,Pid}.

%% @spec stop() -> ok
stop() ->
	?MODULE ! stop,
	ok.

%% @private
loop() ->
	receive
		stop ->
			exit(ok);
		
		{request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams} ->
			request(ReplyDetails, ?TIMEOUT, Auth, Method, MandatoryParams, OptionalParams);

		{request, ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams} ->
			request(ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams);

		{http, {RequestId, {error, Reason}}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			reply(ReturnDetails, {error, Reason});

		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			reply(ReturnDetails, {response, Result})
		
	end,
	loop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%                      REQUEST HANDLING
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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



%% users/show
%% ==========
%% @private
request(Rd, TO, Auth, users.show, [{user_id, UserId}], []) ->
	doreq(Rd, TO, Auth, get, "users/show/show.xml", [{user_id, UserId}], []);

%% @private
request(Rd, TO, Auth, users.show, [{screen_name, ScreenName}], []) ->
	doreq(Rd, TO, Auth, get, "users/show/show.xml", [{screen_name, ScreenName}], []);


%% statuses/user_timeline
%% ======================
%% @TODO can break down optional parameters further
%%       user_id, screen_name, since_id, max_id, count, page
%% @private
request(Rd, TO, Auth, statuses.user_timeline, [], []) ->
	doreq(Rd, TO, Auth, get, "statuses/user_timeline.xml", [], []);

%% @private
request(Rd, TO, Auth, statuses.user_timeline, [], OpParams) when is_list(OpParams) ->
	io:format("(A)~n",[]),
	doreq(Rd, TO, Auth, get, "statuses/user_timeline.xml", [], OpParams);

%% @private
request(Rd, _TO, _Auth, statuses.user_timeline, _, _) ->
	reply(Rd, {method_error, statuses.user_timeline});

%% statuses/update
%% ===============
%% @private
request(Rd, TO, Auth, statuses.update, [{status, Status}], OpParams) ->
	doreq(Rd, TO, Auth, post, "statuses/update.xml", [{status, Status}], OpParams);

%% @private
request(Rd, _TO, _Auth, statuses.update, _Params, _OpParams) ->
	reply(Rd, {method_error, statuses.update});


%% >>>> Catch-all <<<<
%% @private
request(ReturnDetails, _Timeout, _Auth, Method, _MandatoryParams, _OptionalParams) ->
	reply(ReturnDetails, {unknown_method, Method}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
doreq(Rd, Timeout, Auth, get, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	PL=?TOOLS:encode_list(Params),
	Req=Method++?TOOLS:format_encoded_list(PL),
	do_auth_request(Rd, Timeout, get, Req, Auth);

%% @private
doreq(Rd, Timeout, Auth, post, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	Body=?TOOLS:encode_list(Params),
	do_auth_request(Rd, Timeout, post, Method, Auth, "application/x-www-form-urlencoded", Body).



%% @private
do_auth_request(ReturnDetails, Timeout, Req, {auth, Username, Password}) ->
	do_auth_request(ReturnDetails, Timeout, get, Req, Username, Password).

%% @private
do_auth_request(ReturnDetails, Timeout, Type, Req, {auth, Username, Password}) ->
	Auth=?TOOLS:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Timeout, Req, [{"authorization", Auth}]).

%% @private
do_auth_request(ReturnDetails, Timeout, Type, Req, Username, Password) ->
	Auth=?TOOLS:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Timeout, Req, [{"authorization", Auth}]).

%% @private
do_auth_request(ReturnDetails, Timeout, Type, Req, {auth, Username, Password}, ContentType, Body) ->
	Auth=?TOOLS:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Timeout, Req, [{"authorization", Auth}], ContentType, Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @private
do_request(ReturnDetails, Timeout, Req, Headers) ->
	do_request(get, ReturnDetails, Timeout, Req, Headers).

%% @private
do_request(Type, ReturnDetails, Timeout, Req, Headers) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		%% Response will be messaged
		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.

%% @private
do_request(Type, ReturnDetails, Timeout, Req, Headers, ContentType, Body) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers, ContentType, Body}, [{timeout, Timeout}], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			put({requestid, RequestId}, ReturnDetails);
	
		{error, Reason} ->
			reply(ReturnDetails, {request_error, Reason})
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	

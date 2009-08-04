%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter API
%%
%% Support for additional methods can be easily added.
%%
%% Use tools:extract to retrieve a specific field
%%  of the result-set returned by twitter_api:request
%%

-module(twitter_api).

%%
%% Exported Functions
%%
-export([
		 request/5
		 ]).

-define(API,     "http://twitter.com/").
-define(TIMEOUT, 5000).

%% LOCAL
-export([
		 do_auth_request/3,
		 do_auth_request/4,
		 do_request/3,
		 do_request/4,
		 wait_for_response/2,
		 
		 %% Testing
		 test/0,
		 test_update/0
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      REQUEST HANDLING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% users/show
%% ==========
request(Rd, Auth, users.show, [{user_id, UserId}], []) ->
	doreq(Rd, Auth, get, "users/show/show.xml", [{user_id, UserId}], []);

request(Rd, Auth, users.show, [{screen_name, ScreenName}], []) ->
	doreq(Rd, Auth, get, "users/show/show.xml", [{screen_name, ScreenName}], []);


%% statuses/user_timeline
%% ======================
%% @TODO can break down optional parameters further
%%       user_id, screen_name, since_id, max_id, count, page
request(Rd, Auth, statuses.user_timeline, [], []) ->
	doreq(Rd, Auth, get, "statuses/user_timeline.xml", [], []);

request(Rd, Auth, statuses.user_timeline, [], OpParams) when is_list(OpParams) ->
	io:format("(A)~n",[]),
	doreq(Rd, Auth, get, "statuses/user_timeline.xml", [], OpParams);

request(Rd, _Auth, statuses.user_timeline, _, _) ->
	return_code(Rd, {method_error, statuses.user_timeline});

%% statuses/update
%% ===============
request(Rd, Auth, statuses.update, [{status, Status}], OpParams) ->
	doreq(Rd, Auth, post, "statuses/update.xml", [{status, Status}], OpParams);

request(Rd, _Auth, statuses.update, _Params, _OpParams) ->
	return_code(Rd, {method_error, statuses.update});


%% >>>> Catch-all <<<<
request(ReturnDetails, _Auth, Method, _MandatoryParams, _OptionalParams) ->
	return_code(ReturnDetails, {unknown_method, Method}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




doreq(Rd, Auth, get, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	PL=tools:encode_list(Params),
	Req=Method++tools:format_encoded_list(PL),
	do_auth_request(Rd, get, Req, Auth);

doreq(Rd, Auth, post, Method, MandatoryParams, OpParams) ->
	Params=lists:append(MandatoryParams, OpParams),
	Body=tools:encode_list(Params),
	do_auth_request(Rd, post, Method, Auth, "application/x-www-form-urlencoded", Body);

doreq(_Rd, _Auth, HttpMethod, Method, Params, OpParams) ->
	{HttpMethod, Method, Params, OpParams}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_auth_request(ReturnDetails, Req, {auth, Username, Password}) ->
	do_auth_request(ReturnDetails, get, Req, Username, Password).

do_auth_request(ReturnDetails, Type, Req, {auth, Username, Password}) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, [{"authorization", Auth}]).

do_auth_request(ReturnDetails, Type, Req, Username, Password) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, [{"authorization", Auth}]).

do_auth_request(ReturnDetails, Type, Req, {auth, Username, Password}, ContentType, Body) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, [{"authorization", Auth}], ContentType, Body).




do_request(ReturnDetails, Req, Headers) ->
	do_request(get, ReturnDetails, Req, Headers).

do_request(Type, ReturnDetails, Req, Headers) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, Headers}, [], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			wait_for_response(ReturnDetails, RequestId);
	
		{error, Reason} ->
			return_code(ReturnDetails, Reason)
	end.

do_request(Type, ReturnDetails, Req, Headers, ContentType, Body) ->
	CompleteReq=?API++Req,
	io:format("body [~p]~n", [Body]),
	Ret = http:request(Type, {CompleteReq, Headers, ContentType, Body}, [], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			wait_for_response(ReturnDetails, RequestId);
	
		{error, Reason} ->
			return_code(ReturnDetails, Reason)
	end.



wait_for_response(ReturnDetails, RequestId) ->
	receive
		
		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			return_code(ReturnDetails, Result);
		
		{error, Reason}  ->
			return_code(ReturnDetails, Reason);
		
		Other ->
			io:format("wait_for_response: [~p]~n", [Other]),
			return_code(ReturnDetails, wait_error)
	
	after ?TIMEOUT ->
		
		return_code(ReturnDetails, timeout)
			
	end.
	


%% For test purposes mainly
return_code(undefined, Code) ->
	io:format("return_code: [~p]~n", [Code]);

return_code(ReturnDetails, Code) ->
	{From, ReturnCode} = ReturnDetails,
	From ! {ReturnCode, error, Code}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% request(Rd, Auth, users.show, [{user_id, UserId}], []) ->
test() ->
	inets:start(),
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	Param=os:getenv("twitter_param"),
	request(undefined, {auth, Username, Password}, statuses.user_timeline, [], [{screen_name, Param}]),
	ok.

test_update() ->
	inets:start(),
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	Param=os:getenv("twitter_param2"),
	request(undefined, {auth, Username, Password}, statuses.update, [{status, Param}],[]),
	ok.
	


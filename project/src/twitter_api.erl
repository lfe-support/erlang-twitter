%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to twitter_api
-module(twitter_api).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 request/4
		 ]).

-define(API,     "http://twitter.com/").
-define(TIMEOUT, 5000).
-define(STATUSES_USER_TIMELINE_USER_ID,     "statuses/user_timeline.xml?user_id=").
-define(STATUSES_USER_TIMELINE_SCREEN_NAME, "statuses/user_timeline.xml?screen_name=").

-define(UPDATE_STATUS,                      "statuses/update.xml").

%% LOCAL
-export([
		 do_auth_request/3,
		 do_auth_request/4,
		 do_request/3,
		 do_request/4,
		 wait_for_response/2,
		 test/0,
		 test_update/0
		 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test() ->
	inets:start(),
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	Param=os:getenv("twitter_param"),
	request(undefined, {auth, Username, Password}, statuses.user_timeline, [{"screen_name", Param}]),
	ok.

test_update() ->
	inets:start(),
	Username=os:getenv("twitter_username"),
	Password=os:getenv("twitter_password"),
	Param=os:getenv("twitter_param"),
	request(undefined, {auth, Username, Password}, statuses.update, {status, Param}),
	ok.
	

%% users/show
%% ==========
request(Rd, Auth, users.show, Params) ->
	PL=tools:encode_list(Params),
	Req="users/show/show.xml"++tools:format_encoded_list(PL),
	do_auth_request(Rd, Req, Auth);

%% statuses/user_timeline
%% ======================
request(Rd, Auth, statuses.user_timeline, Params) ->
	PL=tools:encode_list(Params),
	Req="statuses/user_timeline.xml"++tools:format_encoded_list(PL),
	do_auth_request(Rd, Req, Auth);

%% statuses/update
%% ===============
request(Rd, Auth, statuses.update, {status, Status}) ->
	Req="statuses/update.xml",
	EncodedBody=tools:encode_tuple("status", Status),
	do_auth_request(Rd, post, Req, Auth, "text/plain", EncodedBody);


%% Catch-all
request(ReturnDetails, _Auth, Method, _Params) ->
	return_code(ReturnDetails, {unknown_method, Method}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LOCAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_auth_request(ReturnDetails, Req, {auth, Username, Password}) ->
	do_auth_request(ReturnDetails, get, Req, Username, Password).

do_auth_request(ReturnDetails, Type, Req, {auth, Username, Password}) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, Auth).

do_auth_request(ReturnDetails, Type, Req, Username, Password) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, Auth).

do_auth_request(ReturnDetails, Type, Req, {auth, Username, Password}, ContentType, Body) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(Type, ReturnDetails, Req, Auth, ContentType, Body).




do_request(ReturnDetails, Req, Auth) ->
	do_request(get, ReturnDetails, Req, Auth).

	
do_request(Type, ReturnDetails, Req, Auth) ->
	%%io:format("Authorization [~p]~n", [Auth]),
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, [{"Authorization", Auth}]}, [], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			wait_for_response(ReturnDetails, RequestId);
	
		{error, Reason} ->
			return_code(ReturnDetails, Reason)
	end.

do_request(Type, ReturnDetails, Req, Auth, ContentType, Body) ->
	CompleteReq=?API++Req,
	Ret = http:request(Type, {CompleteReq, [{"Authorization", Auth}], ContentType, Body}, [], [{sync, false}]),
	case Ret of

		{ok, RequestId} ->
			wait_for_response(ReturnDetails, RequestId);
	
		{error, Reason} ->
			return_code(ReturnDetails, Reason)
	end.
	

%%
%% Local Functions
%%
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



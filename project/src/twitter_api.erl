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
		 statuses_user_timeline/4
		 %%show_status/4
		 %%update_status/4
		 ]).

-define(TIMEOUT,       5000).
-define(STATUSES_USER_TIMELINE_USER_ID,     "http://twitter.com/statuses/user_timeline.xml?user_id=").
-define(STATUSES_USER_TIMELINE_SCREEN_NAME, "http://twitter.com/statuses/user_timeline.xml?screen_name=").

-define(UPDATE_STATUS,                      "http://twitter.com/statuses/update.xml").


-export([
		 wait_for_response/2,
		 test/0
		 ]).

%%
%% API Functions
%%
test() ->
	inets:start(),
	
	ok.


statuses_user_timeline(ReturnDetails, Username, Password, UserId) when is_integer(UserId) ->
	Req=?STATUSES_USER_TIMELINE_USER_ID++UserId,
	do_auth_request(ReturnDetails, Req, Username, Password);

statuses_user_timeline(ReturnDetails, Username, Password, ScreenName) when is_list(ScreenName) ->
	Req=?STATUSES_USER_TIMELINE_SCREEN_NAME++ScreenName,
	do_auth_request(ReturnDetails, Req, Username, Password).




do_auth_request(ReturnDetails, Req, Username, Password) ->
	Auth=tools:gen_auth_header(Username, Password),
	do_request(ReturnDetails, Req, Auth).
	

do_request(ReturnDetails, Req, Auth) ->
	%%io:format("Authorization [~p]~n", [Auth]),
	Ret = http:request(get, {Req, [{"Authorization", Auth}]}, [], [{sync, false}]),
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



%% Author: Jean-Lou Dupont
%% Created: 2009-08-20
%% Description: Twitter API
%%
-module(twitter_api).

-compile(export_all).

-export([
		 req/5, req/6
		 ]).

%%
%% Macros
%%
-define(SERVER, twitter).
-define(REQ,    twitter_req).


%% ----------------------                     ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REQUEST DISPATCHING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                     ------------------------------


%% @doc Request dispatching.
%% The parameter 'From' identifies the recipient 'server' whilst the 'Context'
%%  parameter serves as identifier in the response to the server. The response
%%  format is as follows:
%%  ```
%%    {Context, Message}
%%  '''
%%
%% @spec req(ReplyDetails, Auth, Method, MandatoryParams, OptionalParams) -> ok | error
%% where
%%		ReplyDetails = {From, Context}
%%			From = atom() | pid() 
%%			Context = atom()
%%		Auth = {auth, Username, Password}
%%			Username = string()
%%			Password = string()
%%		Method = atom()
%%		MandatoryParams = nil() | [tuple()]
%%		OptionalParams  = nil() | [tuple()]
%%
%%
req(ReplyDetails, Auth, Method, MandatoryParams, OptionalParams) ->
	Ret = ?SERVER ! {request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams},
	case Ret of
		{request, ReplyDetails, Auth, Method, MandatoryParams, OptionalParams} ->
			ok;
		_ ->
			error
	end.

%% @doc
%% Same as req/5 expect for the Timeout parameter expressed in milliseconds.
req(ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams) ->
	Ret = ?SERVER ! {request, ReplyDetails, Timeout, Auth, Method, MandatoryParams, OptionalParams},
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
	?REQ:doreq(Rd, TO, Auth, get, "users/show/show.xml", [{user_id, UserId}], []);

%% @private
request(Rd, TO, Auth, users.show, [{screen_name, ScreenName}], []) ->
	?REQ:doreq(Rd, TO, Auth, get, "users/show/show.xml", [{screen_name, ScreenName}], []);


%% statuses/user_timeline
%% ======================
%% @TODO can break down optional parameters further
%%       user_id, screen_name, since_id, max_id, count, page
%% @private
request(Rd, TO, Auth, statuses.user_timeline, [], []) ->
	?REQ:doreq(Rd, TO, Auth, get, "statuses/user_timeline.xml", [], []);

%% @private
request(Rd, TO, Auth, statuses.user_timeline, [], OpParams) when is_list(OpParams) ->
	io:format("(A)~n",[]),
	?REQ:doreq(Rd, TO, Auth, get, "statuses/user_timeline.xml", [], OpParams);

%% @private
request(Rd, _TO, _Auth, statuses.user_timeline, _, _) ->
	?REQ:reply(Rd, {method_error, statuses.user_timeline});

%% statuses/update
%% ===============
%% @private
request(Rd, TO, Auth, statuses.update, [{status, Status}], OpParams) ->
	?REQ:doreq(Rd, TO, Auth, post, "statuses/update.xml", [{status, Status}], OpParams);

%% @private
request(Rd, _TO, _Auth, statuses.update, _Params, _OpParams) ->
	?REQ:reply(Rd, {method_error, statuses.update});

%% Account Rate Limit Status
%%
%% @private
request(Rd, TO, Auth, account.rate_limit_status, [], []) ->
	?REQ:doreq(Rd, TO, Auth, post, "account/rate_limit_status.xml", [], []);

request(Rd, _TO, _Auth, account.rate_limit_status, _M, _O) ->
	?REQ:reply(Rd, {method_error, account.rate_limit_status});



%% >>>> Catch-all <<<<
%% @private
request(ReturnDetails, _Timeout, _Auth, Method, _MandatoryParams, _OptionalParams) ->
	?REQ:reply(ReturnDetails, {unknown_method, Method}).



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
	

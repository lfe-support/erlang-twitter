%% Author: Jean-Lou Dupont
%% Created: 2009-08-20
%% Description: Twitter API
-module(twitter_api).

-compile(export_all).

%%
%% Macros
%%
-define(REQ, twitter_req).



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

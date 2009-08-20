%% Author: Jean-Lou Dupont
%% Created: 2009-08-20
%% Description: TODO: Add description to twitter_req
-module(twitter_req).

-compile(export_all).

%%
%% Macros
%%
-define(API,     "http://twitter.com/").
-define(TOOLS, twitter_tools).

%%
%% Exported Functions
%%
-export([]).


%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REQUEST WORKERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


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


%% @private
reply(undefined, Message) ->
	io:format("~p:reply(~p)~n", [?MODULE, Message]),
	Message;

%% @private
reply({From, Context}, Message) ->
	From ! {Context, Message}.

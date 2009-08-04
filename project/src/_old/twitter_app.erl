%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter daemon
%%
-module(twitter_app).

%%
%% Include files
%%
-include("../include/twitter.hrl").

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start_link/0,
		 start_link/1
		 ]).

-export([
		 loop/1,
		 rpc/1
		]).

%%
%% API Functions
%%
start() ->
	start_link([]).

start_link() ->
	start_link([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request(Method) ->
	rpc({request, {auth, undefined, undefined}, Method, [], []}).

request(Method, Params) ->
	rpc({request, {auth, undefined, undefined}, Method, Params, []}).

request({auth, Username, Password}, Method, Params) ->
	rpc({request, {auth, Username, Password}, Method, Params, []}).
  
request({auth, Username, Password}, Method, Params, OpParams) ->
	rpc({request, {auth, Username, Password}, Method, Params, OpParams}).

%% Management
pong() ->
	io:format("twitter_app:pong~n"),
	rpc(pong).
	
ping() ->
	io:format("twitter_app:ping~n"),
	rpc(ping).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     LOCAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpc(Q) ->
	io:format("twitter_app: rpc(~p)~n", [Q]),
	twitter ! {self(), Q},
	receive
		{twitter, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			rpcerror
	
	after 2000 ->
			
			io:format("~p: rpc timeout~n",[?MODULE]),
			rpcerror
	end.


%%
%% Local Functions
%%
start_link(Args) ->
	Pid = spawn_link(?MODULE, loop, [Args]),
	register(twitter, Pid),
	io:format("~p daemon started, pid[~p]~n", [?MODULE, Pid]),
	inets:start(),
	{ok, Pid}.



loop(Args) ->
	receive
		
		%% daemon management related
		{From, pong} ->
			io:format("twitter: received 'pong' from [~p]~n", [From]),
			From ! {twitter, {ping, os:getpid()}};
			
		{From, ping} ->
			io:format("twitter: received 'ping' from [~p]~n", [From]),
			From ! {twitter, {pong, os:getpid()}};
		
		{From, {request, Auth, Method, Params, OpParams}} ->
			twitter_api:request({From, twitter}, Auth, Method, Params, OpParams)
	
	end,
	loop(Args).

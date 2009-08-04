%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: Twitter daemon
%%
-module(twitter_app).

%%
%% Include files
%%
-include("twitter.hrl").

%%
%% Exported Functions
%%
-export([
		 start/0,
		 start_link/0,
		 start_link/1,
		 
		 stop/0
		 ]).

-export([
		 loop/1,
		 rpc/1
		]).

-export([
		 request/1,  %% only  Method
		 request/2,  %%       Method, Params
		 request/3,  %% Auth, Method, Params
		 request/4   %% Auth, Method, Params, [optional params]
		 ]).

%%
%% API Functions
%%
start() ->
	start_link([]).

start_link() ->
	start_link([]).

stop() ->
	?MODULE ! stop.


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
ping() ->
	io:format("twitter:ping~n"),
	rpc(ping).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     LOCAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rpc(Q) ->
	io:format("twitter: rpc(~p)~n", [Q]),
	?MODULE ! {self(), Q},
	receive
		{twitter, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			rpcerror
	
	after 2000 ->
			
			rpcerror
	end.


%%
%% Local Functions
%%
start_link(Args) ->
	
	Pid = spawn_link(?MODULE, loop, [Args]),
	register(?MODULE, Pid),
	io:format("~p daemon started, pid[~p]~n", [?MODULE, Pid]),
	inets:start(),
	{ok, Pid}.



loop(Args) ->
	receive
		stop ->
			exit(ok);
	
		%% daemon management related
		{From, ping} ->
			From ! {twitter, {pong, self()}};
		
		{From, {request, Auth, Method, Params, OpParams}} ->
			twitter_api:request({From, twitter}, Auth, Method, Params, OpParams)
	
	end,
	loop(Args).


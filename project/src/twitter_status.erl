%% Author: Jean-Lou Dupont
%% Created: 2009-09-02
%% Description: 
%%
%% @doc 
%%
%% ==Twitter Status==
%%
%% <ul>
%%  <li>Listen on 'tweet' bus for 'tweet.accounts' messages</li>
%%  <li>Send status updates using 'tweet.status' messages</li>
%%  <li></li>
%% </ul>
%%
%% == Logging Contexts ==
%%
%% {twitter.status,request, Aid}
%%
-module(twitter_status).

-define(SERVER,  status).
-define(SWITCH,  twitter_hwswitch).
-define(BUSSES,  [sys, clock, tweet]).
-define(CTOOLS,  twitter_ctools).
-define(REQ,     twitter_req).
-define(API,     twitter_api).
-define(TIMEOUT, 5*1000).

%%
%% API Exported Functions
%%
-export([
		 start_link/0
		 ,stop/0
		 ,get_server/0, get_busses/0
		
		 %% keep compilation warning free
		,loop/0
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).

%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  Management  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
get_server() ->	?SERVER.
get_busses() -> ?BUSSES.


start_link() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! start,
	{ok, Pid}.


stop() ->
	try ?SERVER ! stop, ok
	catch _:_ -> {error, cannot_stop}
	end.


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		start ->
			%% start in the process' context
			inets:start(httpc, [{profile, ?SERVER}]);
		
		{timer, Aid} ->
			do_status(Aid);
		
		
		{config, Version, Config} ->
			%io:format("status:configdata: ~p~n", [Config]),
			?CTOOLS:put_config(Version, Config);
		
		stop ->
			exit(normal);
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});

		
		%%%%%%%%%%% REPLY FROM TWITTER API %%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%                        %%%%%%%%%%%%%%%%%%%
		
		
		{http, {RequestId, {error, Reason}}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			report_error(ReturnDetails, Reason);
			

		%% Result = {{HttpVersion, HttpCode, HttpResponseCode}, [Headers], ResponseBody}
		%% HttpVersion = string()         (eg. "HTTP/1.1")
		%% HttpCode = integer()           (eg. "200")
		%% HttpResponseCode = string()    (eg. "OK")
		%% Headers = {key, value}, {key, value} ...
		%% ResponseBody = string()
		{http, {RequestId, Result}} ->
			ReturnDetails=get({requestid, RequestId}),
			erase({requestid, RequestId}),
			process_result(ReturnDetails, Result);
		
		
		{{Aid, 'account.rate_limit_status'}, Msg} ->
			io:format("status: receive for aid[~p] msg[~p]~n", [Aid, Msg]);
		
		
		Other ->
			log(warning, "updater: unexpected message: ", [Other])
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  PROCESS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  RESULTS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


report_error(ReturnDetails, Reason) ->
	{Aid, _Req} = ReturnDetails,
	log({twitter.status,request, Aid}, error, "status: request to Twitter failed, {AccountId, Reason}: ", [[Aid, Reason]]).


process_result(_ReturnDetails, Result) ->
	io:format("status: reply: ~p~n", [Result]),
	ok.


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%%%%%%%%%%%%%% CLOCK bus %%%%%%%%%%%%%%%%%%%%%%%%


handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;

%%%%%%%%%%%%%% SYS bus %%%%%%%%%%%%%%%%%%%%%%%%%%%

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);


handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle({hwswitch, _From, sys, _Msg}) ->
	%io:format("app: rx sys msg[~p]~n", [Msg]);
	not_supported;

%%%%%%%%%%%%%% TWEET bus %%%%%%%%%%%%%%%%%%%%%%%%%%%

handle({hwswitch, _From, tweet, {accounts, Accounts}  }) ->
	put(accounts, Accounts),
	sync(Accounts);


handle({hwswitch, _From, tweet, _  }) ->
	unsupported;



%%%%%%%%%%%%%%%%%%% --- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% --- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle(Other) ->
	log(warning, "status: Unexpected message: ", [Other]).



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  SYNC  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

%% @doc Go through the accounts list and start (restart)
%%		status polling for each.
%%
sync(Accounts) when is_list(Accounts)->
	do_sync(Accounts);

sync(Other) ->
	log(critical, "status: sync: ", [Other]).

do_sync([]) ->	finished;

do_sync([Account|Accounts]) ->
	sync1(Account),
	do_sync(Accounts).

sync1({Id, User, Pass}) ->
	put({account, Id}, {User, Pass}),
	manage_timer(Id);


sync1(Other) ->
	log(debug, "status: invalid account: ", [Other]).



% request(Rd, TO, Auth, account.rate_limit_status, [], [])
do_status(Aid) ->
	Ad=get({account, Aid}),
	do_status(Aid, Ad).

do_status(Aid, {User, Pass}) ->
	Rd={self(), {request, Aid, 'account.rate_limit_status'}},
	?API:request(Rd, ?TIMEOUT, {auth, User, Pass}, account.rate_limit_status, [], []);


do_status(Aid, Other) ->
	log(critical, "status: invalid account details {AccountId, Details} ", [Aid, Other]).





manage_timer(Aid) ->
	OTRef=get({timer, Aid}),
	timer:cancel(OTRef),
	
	Poll=get_poll(),
	{ok, TRef}=timer:send_interval(Poll, {timer, Aid}),
	put({timer, Aid}, TRef),
	?SERVER ! {timer, Aid}.

	

get_poll() ->
	Poll=get('status.poll'),
	case Poll of
		% just in case
		undefined -> 5*60*1000;
		Other     -> Other
	end.





%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
set_state(State) ->
	put(state, State).

%get_state() ->
%	State=get(state),
%	case State of
%		undefined -> working;   %start in 'working' state
%		working   -> working;
%		_         -> suspended
%	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

log(Ctx, Severity, Msg, Params) ->
	?SWITCH:publish(log, {Ctx, {Severity, Msg, Params}}).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  CONFIG  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc List of parameters which cannot be customized
%%
%% @spec blacklist() -> list()
%%
blacklist() ->
	[].

%% @doc List of default parameters for the module
%%
%% @spec defaults() -> list()
%%
defaults() ->
	[
	  {status.poll,     optional, int, 5*60*1000}  %% in milliseconds
	 ,{status.poll.min, optional, int, 1*60*1000}  %% in milliseconds
	 ,{status.poll.max, optional, int, 15*60*1000} %% in milliseconds
	 ].


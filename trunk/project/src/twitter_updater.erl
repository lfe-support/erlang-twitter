%% Author: Jean-Lou Dupont
%% Created: 2009-09-04
%% Description: 
%%
%% @doc
%%
%% = Updater Agent =
%%
%% 1) (normal) Listen on the 'tweet' bus
%%    and send 'statuses.update' through Twitter API
%%
%% 2) (bypass) Listen on the 'notif' bus
%%    and send 'statuses.update' through Twitter API
%%
%% 3) Send 'tweet.echo' 
%%
%% 4) Listen to 'tweet.accounts'
%%
%%
%% == Bypass path ==
%%
%% Normally, this agent would be used in conjunction with
%% a policer in order to respect the 'rate limits' of Twitter.
%% There is a 'bypass' path which can be configured:
%%
%% ```
%%	{updater.bypass, true}
%% '''
%%
-module(twitter_updater).

-define(SERVER, updater).
-define(BUSSES, [sys, clock, notif, tweet]).
-define(SWITCH, twitter_hwswitch).
-define(CTOOLS, twitter_ctools).
-define(TOOLS,  twitter_tools).
-define(REP,    twitter_response).
-define(API,    twitter_api).
-define(TIMEOUT, 5*1000).


%%
%% API functions
%%
-export([
		 start_link/0
		,get_server/0
		,get_busses/0
		,stop/0
		 ]).

-export([
		 loop/0
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
	try   ?SERVER ! stop, ok
	catch _:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		start ->
			%% start in the process' context
			inets:start(httpc, [{profile, ?SERVER}]);
		
		stop ->
			exit(normal);
	
		{config, Version, Config} ->
			?CTOOLS:put_config(Version, Config);
		
		
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
		
		
		Other ->
			log(warning, "updater: unexpected message: ", [Other])
	end,
	loop().


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------


%%%%%%%%%%%%%% SYS bus
%---------------------

handle({hwswitch, _From, sys, {config, VersionInForce}}) ->
	?CTOOLS:do_config(?SWITCH, ?SERVER, VersionInForce);

handle({hwswitch, _From, sys, suspend}) ->	set_state(suspended);
handle({hwswitch, _From, sys, resume}) ->	set_state(working);

handle({hwswitch, _From, sys, _}) ->
	not_supported;


%%%%%%%%%%%%%% CLOCK bus
%-----------------------

handle({hwswitch, _From, clock, {tick.min, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);

handle({hwswitch, _From, clock, {tick.sync, _Count}}) ->
	?CTOOLS:do_publish_config_version(?SWITCH, ?SERVER);	

handle({hwswitch, _From, clock, _}) ->
	not_supported;


%%%%%%%%%%%%%% NOTIF bus
%-----------------------

handle({hwswitch, From, notif, {Priority, Message} }) ->
	hnotif(From, Priority, Message);

handle({hwswitch, _From, notif, _ }) ->
	unsupported;


%%%%%%%%%%%%%% TWEET bus
%-----------------------

handle({hwswitch, _From, tweet, {accounts, Accounts} }) ->
	put(accounts, Accounts);
	

handle({hwswitch, _From, tweet, _}) ->
	unsupported;



%%%%%%%%%%%%%% CATCH-ALL
%-----------------------

handle(Other) ->
	log(warning, "snooper: unexpected message: ", [Other]).



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  DOERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

hnotif(From, Priority, Message) ->
	Account=pick_account(),
	State=get_state(),
	maybe_send_update(State, From, Priority, Message, Account).

maybe_send_update(working, _From, _Priority, Message, {Aid, Username, Password}) ->
	Rd={Aid, 'statuses.update'},
	?API:request(Rd, ?TIMEOUT, {auth, Username, Password}, statuses.update, [{status, Message}], []);

maybe_send_update(_, _From, _Priority, _Message, Other) -> 
	log(error, "updater: wrong account info: ", [Other]).


	



report_error(ReturnDetails, Reason) ->
	{Aid, _Req} = ReturnDetails,
	log({twitter.updater,request, Aid}, error, "updater: request to Twitter failed, {AccountId, Reason}: ", [[Aid, Reason]]).


process_result({Aid, _}, Result) ->
	PrResult=?REP:process(Result),
	io:format("updater: reply: ~p~n", [PrResult]),
	maybe_publish_result(Aid, PrResult);

process_result(Rd, _) ->
	log(critical, "updater: process_result exception, Rd: ", [Rd]).
	

maybe_publish_result(_Aid, {error, _}) ->	error;

maybe_publish_result(Aid, PrResult) ->
	AccountName=get_account_name(Aid),
	io:format("updater: publish_result: aid[~p] result:~p~n", [Aid, PrResult]),
	?SWITCH:publish(tweet, {echo, {update, Aid, AccountName, PrResult}}).



get_account_name(Aid) ->
	Accounts=get(accounts),
	Entry=?TOOLS:kfind(Aid, Accounts),
	case Entry of
		{_Aid, Username, _Password} ->
			Username;
		_ -> undefined
	end.


%% Algorithm is basic at the moment: pick account Aid=1
pick_account() ->
	Accounts=get(accounts),
	Entry=?TOOLS:kfind(1, Accounts),
	case Entry of
		{Aid, Username, Password} ->
			{Aid, Username, Password};
		_ -> undefined
	end.
	


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------
set_state(State) ->
	put(state, State).

get_state() ->
	State=get(state),
	case State of
		undefined -> working;   %start in 'working' state
		working   -> working;
		_         -> suspended
	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

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
	 %% bypass active by default currently... no policer defined yet.
	 {updater.bypass, optional, atom, true}
	
	 %% Echo the reply following 'statuses.update' API
	,{updater.echo,   optional, atom, true}
	 ].


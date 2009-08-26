% Author: Jean-Lou Dupont
%% Created: 2009-08-25
%% Description: Message Switch with publish/subscribe strategy
%%
%% @doc
%% == DEPENDENCIES ==
%%  A 'logger' process for receiving log messages of the form:
%%
%%	```
%%    {log, Severity, Msg, Params}
%%	'''
%%
%% == Reply Messages ==
%% Reply messages are generated as a result of using API functions:
%%
%% === Subscribe ===
%% ```
%%  {switch, subscribed}
%% '''
%%
%% === Publish ===
%%
%% When using the 'publish' API function, the following message is 
%% sent back to the caller iff this module detects that an 'out-of-sync'
%% condition prevails.
%%
%% ```
%%	{switch, out_of_sync}
%% '''
%%
%% === Common ===
%% The following message is sent back to the caller on this condition
%% of the switch not being available for receiving the request. 
%% ```
%% 	{switch, unreachable}
%% '''
%% This condition would typically occur when the switch hasn't been started
%% or is in the process of being restarted.
%%
%% == Synchronization ==
%%
%% A 'client' module (one that uses the 'subscribe' API to receive messages)
%% gets a 'synchronization state' inserted in its process dictionary as a 
%% result of using this module: the variable in question is:
%%
%% ```
%% 	{switch.pid}
%% '''
%%
%% By referring to this caller-side variable, this module can detect when
%% a client falls 'out-of-sync' and thus can inform the client caller of
%% the condition.  The typical action on the client side would be to perform
%% a 're-subscribe' action.
%%
%% === Important Note ===
%%
%% Note that the message '{switch, out_of_sync}' will also be sent when 
%% the switch is first accessed as there is no realistic way of inferring a
%% ''never been used'' condition.
%%
-module(twitter_switch).
-compile({nowarn_unused_function, [
			{compare_switch_pids,3},
			{add_subscriber,2},
			{do_add_subscriber,2},
			{add_type,1},
			{do_publish,3}, {do_publish_list,5},
			{log,2},
			{getvar, 2}, {getvar, 3},
			{add_to_list_no_duplicates,2},
			{tstart,6}, {tloop, 5},
			{kstart,1}, {kloop, 1}
		]}).

%% Timer for dealing with Client/Switch restarts
-define(DEFAULT_TIMER, 5*1000).

-define(SERVER, switch).


%%=====%%
%% API %%
%%=====%%
-export([
		 start_link/0, start_link/1,
		 stop/0,
		 subscribe/1,
		 publish/2
		 ]).

-export([
		 loop_switch/0,
		 timer_tick/2
		 ]).

%% TEST RELATED
%%
-export([
		 test/0, tloop/5, kloop/1
		 ]).

%% @doc Default start with 'logger'
%%
start_link() ->
	start_link([{logger, logger}]).

%% @doc Start with 'logger' parameter
%%
start_link([{logger, LoggerName}]) ->
	Pid = spawn_link(?MODULE, loop_switch, []),
	Pid ! {logger, LoggerName},
	register(?SERVER, Pid),
	{ok, Pid}.

stop() ->
	try
		?SERVER ! stop,
		ok
	catch
		_:_ ->
			{error, cannot_stop}
	end.



%% @doc Subscribe a Client to a Type of message
%%
%% Subscription is not synchronous: the caller will
%% be serviced asynchronously i.e. don't count on
%% the ability to receive message(s) from subscribed
%% types after this function call returns.
%%
%% @spec subscribe(Type) -> void()
%% where
%%	Type = atom()
%%
subscribe(Type) when is_atom(Type) ->
	check_sync(),
	to_switch(self(), subscribe, Type);

subscribe(Types) when is_list(Types) ->
	check_sync(),
	to_switch(self(), subscribe, Types);

subscribe(Other) ->
	{error, {invalid_type, Other}}.
	


%% @doc Publish a Message of a Type
%%
%% @spec publish(From, MsgType, Msg) -> void()
%% where
%%	Type = atom()
%%	Msg = list() | atom() | tuple()
%%
publish(Type, Msg) when is_atom(Type) ->
	check_sync(),
	to_switch(self(), publish, {Type, Msg});

publish(Type, _Msg) ->
	{error, {invalid_type, Type}}.




%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  INTERNAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------


check_sync() ->
	%% clear any outstanding timer
	%% because we are going to set one anyway
	OldTimer=get({switch.timer}),
	erase({switch.timer}),
	timer:cancel(OldTimer),
	set_timer().

set_timer() ->
	SwitchPid=get_pid(?SERVER),
	Result=timer:apply_after(?DEFAULT_TIMER, ?MODULE, timer_tick, [self(), SwitchPid]),
	case Result of
		%% We can't do much here... 
		{error, Reason} -> put({switch, last_error}, Reason), io:format("set_timer: exception[~p]~n",[Reason]);
		{ok, TRef}      -> put(switch.timer, TRef);
		_               -> not_much_we_can_do
	end.


%% @doc Timer expiry handler
%%		Sends a message to the caller iff an 'out-of-sync' condition is detected.
%%
%% @spec timer_tick(CallerPid, PreviousSwitchPid) -> void()
%% where
%%	CallerPid = pid()         % the pid() of the Caller
%%	PreviousSwitchPid = pid() % the pid() of the switch as it was before starting the timer
%%
timer_tick(CallerPid, PreviousSwitchPid) ->
	CurrentSwitchPid=get_pid(?SERVER),
	compare_switch_pids(CallerPid, PreviousSwitchPid, CurrentSwitchPid).

compare_switch_pids(_CallerPid,  _,       undefined) -> switch_unreachable;
compare_switch_pids(CallerPid, undefined, _)         -> send_out_of_sync(CallerPid); % must allow for re-subscribe
compare_switch_pids(_CallerPid, X, X)                -> in_sync;
compare_switch_pids(CallerPid, _, _)                 -> send_out_of_sync(CallerPid).
	
	
send_out_of_sync(CallerPid) ->
	reply(CallerPid, {switch, out_of_sync}).

%% @doc Reply to Caller
%%		Make this as robust as possible since
%%		the Caller could disappear at any time.
%%
%% @spec reply(To, Msg) -> void()
%% where
%%	To = pid()
%%	Msg = atom() | list() | tuple()
%%
reply(To, Msg) ->
	try    To ! Msg
	catch _:_ -> log(critical, "switch: couldn't reply {To, Msg} ", [To, Msg])
	end.



%% @doc Communication bridge with the switch process
%%
to_switch(From, Cmd, Msg) ->
	try ?SERVER ! {From, Cmd, Msg} of
		{From, Cmd, Msg} -> ok
	catch
		_:_ ->
			%% Safely sends a message back to the caller
			reply(From, {switch, unreachable})
			%%log(critical, "switch: fail to switch {Cmd, Msg}: ",[Cmd,Msg])
	end.



loop_switch() ->
	receive
		stop ->
			exit(ok);
		
		{logger, LoggerName}    -> put(logger, LoggerName);
		{From, subscribe, Type} -> add_subscriber(From, Type);
		
		{From, publish, {MsgType, Msg}} -> 
			do_publish(From, MsgType, Msg);
		
		Other -> 
			log(warning, "switch: received unknown message: ", [Other])
	end,
	loop_switch().





%% Shouldn't occur
add_subscriber(Client, undefined) when is_atom(Client) ->
	log(critical, "switch: invalid bus type");

%%
%% @spec add_subscriber(Client, Type)
%%       Client = Atom()
%%       Type   = Atom()
add_subscriber(Client, Type) when is_atom(Type), is_pid(Client) ->
	do_add_subscriber(Client, Type),
	reply(Client, {switch, subscribed});

add_subscriber(Client, []) when is_pid(Client) ->
	reply(Client, {switch, subscribed});

add_subscriber(Client, TypeList) when is_pid(Client), is_list(TypeList) ->
	[H|T] = TypeList,
	do_add_subscriber(Client, H),
	add_subscriber(Client, T);

add_subscriber(_, _) ->
	log(critical, "add_subscriber exception").

do_add_subscriber(Client, Type) when is_atom(Type) ->
	add_to_list_no_duplicates({msgtype, Type}, Client),
	add_type(Type);

do_add_subscriber(_,_) ->
	log(critical, "do_add_subscriber exception").


rem_subscriber(MsgType, CurrentTo) ->
	rem_from_list({msgtype, MsgType}, CurrentTo).



%% @doc Adds a Type to the registered MsgTypes list, no duplicates
%%
add_type(Type) ->
	add_to_list_no_duplicates(msgtypes, Type).




do_publish(From, MsgType, Msg) when is_pid(From), is_atom(MsgType) ->
	ToList = getvar({msgtype, MsgType}, []),
	case ToList of
		[] -> ok;
		_ ->
			[To|Rest] = ToList,
			do_publish_list(To, Rest, From, MsgType, Msg)
	end;

do_publish(From, Type, _Msg) ->
	error_logger:error_msg("~p: do_publish exception, From[~p] Type[~p]~n",[?MODULE, From, Type]).


do_publish_list([], [], From, _MsgType, _Msg) when is_pid(From) -> ok;											  
do_publish_list([], _,  From, _MsgType, _Msg) when is_pid(From) -> ok;											  


do_publish_list(CurrentTo, RestTo, From, MsgType, Msg) when is_pid(From) ->
	try CurrentTo ! {From, MsgType, Msg} of
		{From, MsgType, Msg} -> ok
	catch
		_X:_Y -> 
			io:format("removed [~p]~n", [CurrentTo]),
			rem_subscriber(MsgType, CurrentTo),
			log(warning, "switch: publish error (probably a subscriber died), {To, Type, Msg}", [CurrentTo, MsgType, Msg])			
	end,
	case RestTo of 
		[] -> ok;
		_ ->
			[NewTo|NewRest] = RestTo,
			do_publish_list(NewTo, NewRest, From, MsgType, Msg)
	end;


do_publish_list(_CurrentTo, _RestTo, _From, _MsgType, _Msg) ->
	log(critical, "do_publish_list exception").





%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

log(Severity, Msg) ->
	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	Logger=get(logger),
	try	  Logger ! {log, Severity, Msg, Params}
	catch
		_:_ -> 
			io:format("~p: [~p] ~p ~p ~n",[?MODULE, Severity, Msg, Params])
	end.



%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

%% I know some of these are located also elsewhere
%% but I wanted to make this module as stand-alone as possible

  
getvar(VarName, Default) ->
	VarValue=get(VarName),
	getvar(VarName, VarValue, Default).

getvar(VarName, undefined, Default) ->
	put(VarName, Default),
	Default;

getvar(_VarName, VarValue, _Default) ->
	VarValue.



add_to_list_no_duplicates(List, Element) ->
	ListVar=getvar(List, []),
	FilteredList = ListVar      -- [Element],
	NewListe     = FilteredList ++ [Element],
	put(List, NewListe).

rem_from_list(List, Elements) when is_list(Elements) ->
	ListVar=getvar(List, []),
	FilteredList = ListVar -- Elements,
	put(List, FilteredList);
	
rem_from_list(List, Element) ->
	ListVar=getvar(List, []),
	FilteredList = ListVar -- [Element],
	put(List, FilteredList).


%% @doc Returns the pid() associated with the registered Name
%%		or atom(undefined).
%%
%% @spec get_pid(Name) -> pid() | undefined
%% where
%%	Name = atom()
%%
get_pid(Name) ->
	Pid=erlang:whereis(Name),
	get_pid(Name, Pid).

get_pid(_Name, undefined) ->
	undefined;

get_pid(_Name, Pid) ->
	case erlang:is_process_alive(Pid) of
		true  -> Pid;
		false -> undefined
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

%% 
test() ->
	%%?MODULE:start_link(),
	tstart(n1, 2000, bus1, {msg, "From Proc 1"}, 1, [bus1, bus2]),
	tstart(n2, 2500, bus1, {msg, "From Proc 2"}, 2, [bus1, bus2]),
	tstart(n3, 3000, bus1, {msg, "From Proc 3"}, 3, [bus1, bus2]),
	tstart(n4, 1000, bus2, {msg, "From Proc 4"}, 4, [bus2]),
	tstart(n5, 1000, bus2, {msg, "From Proc 5"}, 5, [bus2]),
	kstart(30000),
	ok.


tstart(Name, Delay, Type, Msg, Id, Busses) ->
	Pid=spawn(?MODULE, tloop, [Delay, Type, Msg, Id, Busses]),
	register(Name, Pid),
	Pid ! start,
	{ok, Pid}.

tloop(Delay, Type, Msg, Id, Busses) ->
	receive
		{From, MsgType, Msg} ->
			io:format("From[~p] MsgType[~p] Msg[~p]~n",[From, MsgType, Msg]);
		
		stop  -> exit(ok);
		start -> subscribe(Busses);

		{switch, out_of_sync} ->
			io:format("out-of-sync received [~p]~n", [now()]),
			subscribe(Busses);
			
		{switch, subscribed} ->
			io:format("subscribed, id[~p]~n", [Id])
	
	after Delay ->
		%%io:format("publishing, id[~p]~n", [Id]),
		publish(Type, Msg)
			
	end,
	tloop(Delay, Type, Msg, Id, Busses).


kstart(Delay) ->
	Pid=spawn(?MODULE, kloop, [Delay]),
	Pid ! start,
	{ok, Pid}.

kloop(Delay) ->
	receive
		start ->
			ok
		
	after Delay ->

		io:format("Attempting switch kill [~p]~n", [now()]),			
		case ?MODULE:stop() of
			ok ->
				ok;
			_ ->
				io:format("Attempting switch restart [~p]~n", [now()]),
				?MODULE:start_link()	
		end
	
	end,	
	kloop(Delay).


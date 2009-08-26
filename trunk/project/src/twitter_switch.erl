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
-compile({nowarn_unused_function, [timer_tick, loop_switch]}).

%% Timer for dealing with Client/Switch restarts
-define(DEFAULT_TIMER, 5*1000).


%%=====%%
%% API %%
%%=====%%
-export([
		 start_link/0, start_link/1,
		 subscribe/2,
		 publish/3
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
	register(?MODULE, Pid),
	{ok, Pid}.


%% @doc Subscribe a Client to a Type of message
%%
%% Subscription is not synchronous: the caller will
%% be serviced asynchronously i.e. don't count on
%% the ability to receive message(s) from subscribed
%% types after this function call returns.
%%
%% @spec subscribe(Type) -> void()
%% where
%%	From = atom() %process registered name
%%	Type = atom()
%%
subscribe(From, Type) when is_atom(From), is_atom(Type) ->
	check_sync(),
	to_switch(From, subscribe, Type).



%% @doc Publish a Message of a Type
%%
%% @spec publish(From, MsgType, Msg) -> void()
%% where
%%	From = atom()  %process registered name
%%	Type = atom()
%%	Msg = list() | atom() | tuple()
%%
publish(From, Type, Msg) when is_atom(From), is_atom(Type) ->
	check_sync(),
	to_switch(From, publish, {Type, Msg}).




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
	SwitchPid=get_pid(switch),
	Result=timer:apply_after(?DEFAULT_TIMER, ?MODULE, timer_tick, [self(), SwitchPid]),
	case Result of
		%% We can't do much here... 
		{error, Reason} -> put({switch, last_error}, Reason);
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
	CurrentSwitchPid=get_pid(switch),
	compare_switch_pids(CallerPid, PreviousSwitchPid, CurrentSwitchPid).

compare_switch_pids(CallerPid, X, X) ->	in_sync;
compare_switch_pids(CallerPid, _, _) -> reply(CallerPid, {switch, out_of_sync}).
	

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
	try ?MODULE ! {From, Cmd, Msg} of
		{From, Cmd, Msg} -> ok
	catch
		_:_ ->
			%% Safely sends a message back to the caller
			reply(From, {switch, unreachable}),
			log(critical, "switch: fail to switch {Cmd, Msg}: ",[Cmd,Msg])
	end.



loop_switch() ->
	receive
		{logger, LoggerName} ->
			put(logger, LoggerName);
		
		%% SWITCH DUTY: subscribe
		{From, subscribe, Type} ->
			add_subscriber(From, Type);
		
		%% SWITCH DUTY: publish
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
add_subscriber(Client, Type) when is_atom(Type), is_atom(Client) ->
	do_add_subscriber(Client, Type),
	reply(Client, {switch, subscribed});

add_subscriber(Client, []) when is_atom(Client) ->
	reply(Client, {switch, subscribed});

add_subscriber(Client, TypeList) when is_atom(Client), is_list(TypeList) ->
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


%% @doc Adds a Type to the registered MsgTypes list, no duplicates
%%
add_type(Type) ->
	add_to_list_no_duplicates(msgtypes, Type).




do_publish(From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	ToList = getvar({msgtype, MsgType}, []),
	case ToList of
		[] -> ok;
		_ ->
			[To|Rest] = ToList,
			do_publish_list(To, Rest, From, MsgType, Msg)
	end.

do_publish_list([], [], From, MsgType, _Msg) when is_atom(From), is_atom(MsgType) -> ok;											  
do_publish_list([], _,  From, MsgType, _Msg) when is_atom(From), is_atom(MsgType) -> ok;											  


do_publish_list(CurrentTo, RestTo, From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	try CurrentTo ! {From, MsgType, Msg} of
		{From, MsgType, Msg} -> ok
	catch
		_X:_Y -> log(warning, "switch: publish error (probably a subscriber died), {To, Type, Msg}", [CurrentTo, MsgType, Msg])			
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
		_:_ -> io:format("~p: cannot access logger",[?MODULE])
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

get_pid(Name, Pid) ->
	case erlang:is_alive(Pid) of
		true  -> Pid;
		false -> undefined
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

%% 


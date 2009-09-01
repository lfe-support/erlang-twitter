%% Author: Jean-Lou Dupont
%% Created: 2009-08-27
%% Description: Hard-wired message switch
%%
%% @doc Message Switch with Hard-Wired publication/subscription table.
%%
%% == API ==
%% Use the @see start_link/1 function with the following list:
%%
%% ```
%%  [{busX, [S1, S2, ...]}, {busN, [Sn1, Sn2...]}...]
%% '''
%%
%%  where 'busX' is a unique atom() representing a communication 'bus'
%%  and 'Sn' is a registered atom() name associated with a Client process (i.e. subscriber to bus).
%%
%% == Message Publication ==
%% Messages are sent to the Subscribers using the following format:
%% ```
%%	 {hwswitch, From, Bus, Msg}
%% '''
%% where 'From' is the pid() of the publisher process.
%%
%% == Usage Note ==
%% This module is meant to be started through a Supervisor.
%%
%% == Switch Down ==
%%
%% If the switch process dies and clients try to send messages
%% through it, the module responds with a {hswitch.bound, Bus, Msg}
%% message back to the caller.
%%
%% == Start-up ==
%%
%% This module can auto-configure itself when provided with a list
%% of modules to query for the required information.
%%
-module(twitter_hwswitch).
-compile({nowarn_unused_function, [
			{hpublish, 4}, {hpublish2, 4}
			,{publish_one, 4} ,{publish_one_safe, 5}
			,{log, 2}, {log, 3}
			,{kfind, 2}, {kfind, 3}
			]}).

%% Keep the compilter happy
-export([
		loop/1
		]).

%% Test related
-export([
		 go/0,
		 test/0,
		 tstart/4
		,tloop/0
		,kstart/1
		,kloop/1
		,get_module_subs/1
		 ]).

-define(SERVER, hwswitch).

%%
%% API Functions
%%
-export([
		start_link/2
		,publish/2, publish/3
		]).

%%@doc Starts this server in debug mode
%% 
%% @spec start_link(subs, Subs) -> {ok, Pid}
%% where
%%	Subs = [{Bus, SubList}]
%%	Bus = atom()
%%	@type SubList = list().  A list of registered names
%%
start_link(subs, Subs) when is_list(Subs) ->
	run_subs(Subs);


%%@doc Starts this server by specifying a Modules list
%% 
%% @spec start_link(mods, Modules, {debug, Debug}) -> {ok, Pid}
%% where
%%	Modules = [atom()]
%%  Debug = on | off
%%
start_link(mods, Modules) when is_list(Modules) ->
	run_mods(Modules);


start_link(_,_) ->
	{error, unknown_params}.


%% @private
run_subs(Subs) ->
	do_start(Subs).

run_mods(Modules) ->
	Subs=get_module_subs(Modules),
	do_start(Subs).


%% @private
do_start(Subs) ->
	Pid=spawn_link(?MODULE, loop, [Subs]),
	register(?SERVER, Pid),
	{ok, Pid}.


%% @doc Publishes a Message on a Bus
%%		Sends a
%%		```
%%			{hwswitch.bounce, Bus, Msg}
%%		'''
%%		back to the caller if the switch is unreachable.
%%
%% @spec publish(Bus, Msg) -> ok | {error, hwswitch.unreachable}
%%
publish(Bus, Msg) ->
	try
		?SERVER ! {publish, self(), Bus, Msg},
		ok
	catch
		_:_ ->
			self() ! {hwswitch.bounce, Bus, Msg},
			{error, hwswitch.unreachable}
	end.


%% @doc Publishes a Message on a Bus using
%%      a specific 'From'. For more information, @see publish/2
%%
publish(From, Bus, Msg) when is_atom(From) or is_pid(From) ->
	try
		?SERVER ! {publish, From, Bus, Msg},
		ok
	catch
		_:_ ->
			self() ! {hwswitch.bounce, Bus, Msg},
			{error, hwswitch.unreachable}
	end.


%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------


loop(Subs) ->
	receive
		stop -> 
			exit(ok);
		
		{debug, Debug} ->
			put(debug, Debug);

		{publish, From, Bus, Msg} ->
			hpublish(Subs, From, Bus, Msg);
		
		Other ->
			log(warning, "hwswitch: unexpected message: ", [Other])
	
	end,
	loop(Subs).



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% PUBLISH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

hpublish([], _From, _Bus, _Msg) ->
	no_subscribers;

hpublish(Subs, From, Bus, Msg) ->
	%io:format("hpublish: bus[~p] subs[~p] ~n",[Bus, Subs]),
	try
		{Bus, ToList}=kfind(Bus, Subs),
		hpublish2(ToList, From, Bus, Msg)
	catch
		X:Y ->
			io:format("hublish exception X[~p] Y[~p]~n", [X,Y]),
			no_subs
	end.


hpublish2([], _From, _Bus, _Msg) ->
	no_more_subscribers;
	
hpublish2([To|Rest], From, Bus, Msg) ->
	publish_one(To, From, Bus, Msg),
	hpublish2(Rest, From, Bus, Msg).


publish_one(X, X, _Bus, _Msg) -> not_to_self;
	%io:format("publish_one: not to self [~p]~n", [X]);

publish_one(To, From, Bus, Msg) ->
	Alive=is_alive(To),
	publish_one_safe(Alive, To, From, Bus, Msg).

	
	
publish_one_safe(false, To, _From, _Bus, _Msg) ->
	log(warning, "switch: publish error (probably a subscriber died), To:", [To]);


publish_one_safe(true, To, From, Bus, Msg) ->
	%%io:format("publish_one: To[~p] From[~p] Bus[~p] Msg[~p]~n",[To, From, Bus, Msg]),
	try To ! {hwswitch, From, Bus, Msg} of
		{hwswitch, From, Bus, Msg} -> ok
	catch
		_X:_Y ->
			%% If this is a recurring condition,
			%% it will get caught upstream in publish_one/4
			error
	end.


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
			%% useful message when in debugging mode
			io:format("~p: [~p] ~p ~p ~n",[?MODULE, Severity, Msg, Params])
	end.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

kfind(_Key, []) ->
	{};

%% @doc Searches through a tuple list for a Key
%%
%% @spec kfind(Key, TupleList) -> {} | {Key, Value}
%% where
%%	Key = atom() | tuple()
%%	TupleList = [tuple()]
%%	Value = term()
%%
kfind(Key, List) ->
	case erlang:is_builtin(lists, keyfind, 3) of
		true  ->
			case lists:keyfind(Key,1,List) of
				false -> {};
				Tuple -> Tuple
			end;

		false ->
			case lists:keysearch(Key,1,List) of
				{value, Value} -> Value;
				_              -> {}
			end
	end.

%% Returns {Key, Default} if not found or {Key, Value} otherwise
kfind(Key, [], Default) ->
	{Key, Default};

kfind(Key, List, Default) ->
	case kfind(Key, List) of
		false        -> {Key, Default};
		{Key, Value} ->	{Key, Value}
	end.


is_alive(Name) ->
	Pid=erlang:whereis(Name),
	is_alive(Name, Pid).

is_alive(_Name, undefined) ->
	false;

is_alive(_Name, Pid) when is_pid(Pid) ->
	erlang:is_process_alive(Pid);

is_alive(_,_) ->
	false.


%% @doc Retrieves the Subscriptions for Modules 
%%
%% @spec get_module_subs(Modules) -> SubsList
%% where
%%	Modules=[atom()].  Registered names
%%	@type SubsList=[atom()].  List of bus names
%%
get_module_subs(Modules) ->
	get_module_subs(Modules, []).

get_module_subs([], Subs) ->
	Subs;

get_module_subs(Modules, Subs) when is_list(Modules) ->
	[ModuleName|Rest]=Modules,
	try
		Busses=erlang:apply(ModuleName, get_busses, []),
		UpdatedSubs=update_subs(ModuleName, Busses, Subs),
		get_module_subs(Rest, UpdatedSubs)
	catch
		_:_ ->
			io:format("hwswitch: no busses found for module[~p]~n", [ModuleName]),
			get_module_subs(Rest, Subs)
	end.
	

%% @doc Update a list of bus subscriptions
%%
%% CurrentSubs=[{BusName, [Subscribers]}]
%%
update_subs(ModuleName, Busses, CurrentSubs) ->
	Server=get_module_server(ModuleName),
	update_subs(ModuleName, Busses, CurrentSubs, Server).


update_subs(_ModuleName, [], CurrentSubs, _Server) ->
	CurrentSubs;

update_subs(ModuleName, Busses, CurrentSubs, Server) when is_list(Busses) ->
	[Bus|Rest] = Busses,
	UpdatedSubs=add_to_tuple_list(CurrentSubs, Bus, Server),
	update_subs(ModuleName, Rest, UpdatedSubs, Server);


update_subs(ModuleName, Other, _CurrentSubs, _Server) ->
	io:format("hwswitch: expecting 'busses' list (got[~p]) for module[~p]~n",[Other, ModuleName]).
	


get_module_server(ModuleName) ->
	try
		erlang:apply(ModuleName, get_server, [])
	catch
		_:_ ->
			io:format("hwswitch: not server found for module[~p]~n", [ModuleName]),
			undefined
	end.



%% @doc Adds (respecting uniqueness) an Element
%%		to a specific tuple in a List
%%
%% @spec add_to_tuple_list(List, TupleName, Element) -> TupleList
%% where
%%	List = list().  Tuple list
%%	TupleName= atom()
%%	Element = term()
%%	@type TupleList = [{ItemName, ItemValue}]
%%	@type ItemName = atom()
%%	@type ItemValue = term()
%%
add_to_tuple_list(List, TupleName, Element) when is_atom(TupleName), is_list(List) ->
	do_add_to_tuple_list(List, TupleName, Element);

add_to_tuple_list(_,_,_) ->	{error, invalid_params}.


%% Trivial case... also makes an example for target return format
do_add_to_tuple_list([], TupleName, Element) when is_list(Element)->
	[{TupleName, Element}];

do_add_to_tuple_list([], TupleName, Element) when is_atom(Element)->
	[{TupleName, [Element]}];


do_add_to_tuple_list(List, TupleName, Element) ->
	case is_list(Element) of
		true  -> E=Element;
		false -> E=[Element]
	end,
	
	Tuple=kfind(TupleName, List),
	case Tuple of
		{} ->
			List++[{TupleName, E}];
		
		{_, Value} ->
			FNewList=List--[Tuple],
			FNewValue=Value--E,
			NewValue=FNewValue++E,
			FNewList++[{TupleName,NewValue}]
	end.






%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

go() ->
	Subs = [{bus1, [n1, n2, n3]}, {bus2, [n4, n5, n6]}],
	?MODULE:start_link(Subs).

%% 
test() ->
	tstart(n1, 3000, bus1, "From Proc 1 "),
	tstart(n2, 3400, bus1, "From Proc 2 "),
	tstart(n3, 3600, bus1, "From Proc 3 "),
	tstart(n4, 3000, bus2, "From Proc 4 "),
	tstart(n5, 4400, bus2, "From Proc 5 "),
	tstart(n6, 5400, bus2, "From Proc 6 "),	
	%kstart(30000),
	ok.


tstart(Name, _Delay, Type, Msg) ->
	Pid=spawn_link(?MODULE, tloop, []),
	Pid ! {start, Name, Type, Msg},
	register(Name, Pid),
	{ok, Pid}.

tloop() ->
	receive
		stop  -> exit(normal);

		{start, Name, Type, Msg} ->
			put(name, Name),
			put(type, Type),
			put(msg, Msg);
		
		{From, MsgType, Message} ->
			Name=get(name),
			io:format("name[~p] From[~p] MsgType[~p] Msg[~p]~n",[Name, From, MsgType, Message]);
		
		Other ->
			io:format("Unknown msg[~p]~n", [Other])
	
	after 1500 ->
		Name=get(name),
		io:format("publishing, from name[~p] pid[~p]~n", [Name, self()]),
		
		Type=get(type),
		Msg=get(msg),
		publish(Type, Msg)
			
	end,
	tloop().


kstart(Delay) ->
	Pid=spawn(?MODULE, kloop, [Delay]),
	%%Pid ! start,
	{ok, Pid}.

kloop(Delay) ->
	receive
		%%start ->
			%%ok
		
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


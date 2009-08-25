% Author: Jean-Lou Dupont
%% Created: 2009-08-25
%% Description: TODO: Add description to twitter_msgswitch
-module(twitter_switch).

-define(LOG, twitter_log).


%%
%% Exported Functions
%%
-export([
		 start_link/0,
		 subscribe/2,
		 publish/3
		 ]).

%% LOCAL
-export([
		 loop_switch/0
		 ]).

start_link() ->
	%% Register ourselves as message switch
	Pid = spawn_link(?MODULE, loop_switch, []),
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
%%	Type = atom()
%%
subscribe(From, Type) when is_atom(From) ->
	to_switch(From, subscribe, Type).



%% @doc Publish a Message of Type
%%
%% @spec publish(From, MsgType, Msg) -> void()
%% where
%%	From = pid()
%%	MsgType = atom()
%%	Msg = list() | atom()
%%
publish(From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	to_switch(From, publish, {MsgType, Msg}).




%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   INTERNAL
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

to_switch(From, Cmd, Msg) ->
	try ?MODULE ! {From, Cmd, Msg} of
		{From, Cmd, Msg} -> ok;
		_Error -> ?LOG:log(critical, "switch: fail to switch {Cmd, Msg}: ",[Cmd,Msg])
	catch
		_X:_Y ->  ?LOG:log(critical, "switch: fail to switch {Cmd, Msg}: ",[Cmd,Msg])
	end.



%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%   SHOULD ONLY BE USED BY THE SWITCH PROCESS
%% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

reply(To, Msg) ->
	try    To ! Msg
	catch _:_ -> ?LOG:log(critical, "switch: couldn't reply {To, Msg} ", [To, Msg])
	end.



%% Shouldn't occur
add_subscriber(Client, undefined) when is_atom(Client) ->
	?LOG:log(critical, "switch: invalid bus type");

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
	add_subscriber(Client, T).




do_add_subscriber(Client, Type) ->
	add_to_list_no_duplicates({msgtype, Type}, Client),
	add_type(Type).



%% Adds a Type to the registered MsgTypes list, no duplicates
add_type(Type) ->
	add_to_list_no_duplicates(msgtypes, Type).




do_publish(From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	ToList = base:getvar({msgtype, MsgType}, []),
	case ToList of
		[] -> ok;
		_ ->
			[To|Rest] = ToList,
			do_publish_list(To, Rest, From, MsgType, Msg)
	end.



%% Finished publishing
do_publish_list([], [], From, MsgType, _Msg) when is_atom(From), is_atom(MsgType) -> ok;											  
do_publish_list([], _,  From, MsgType, _Msg) when is_atom(From), is_atom(MsgType) -> ok;											  


do_publish_list(CurrentTo, RestTo, From, MsgType, Msg) when is_atom(From), is_atom(MsgType) ->
	try CurrentTo ! {From, MsgType, Msg} of
		{From, MsgType, Msg} -> ok;
		_Error -> ?LOG:log(warning, "switch: publish error, {To, Type, Msg}", [CurrentTo, MsgType, Msg])
	catch
		_X:_Y -> ?LOG:log(warning, "switch: publish error, {To, Type, Msg}", [CurrentTo, MsgType, Msg])			
	end,
	case RestTo of 
		[] -> ok;
		_ ->
			[NewTo|NewRest] = RestTo,
			do_publish_list(NewTo, NewRest, From, MsgType, Msg)
	end.






%%
%% Local Functions
%%

loop_switch() ->
	receive
		%% SWITCH DUTY: subscribe
		{From, subscribe, Type} ->
			add_subscriber(From, Type);
		
		%% SWITCH DUTY: publish
		{From, publish, {MsgType, Msg}} ->
			do_publish(From, MsgType, Msg);
	
		Other ->
			?LOG:log(warning, "switch: received unknown message: ", [Other])
	end,
	loop_switch().


%% ===============================================================================================
%% LOCAL FUNCTIONS
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

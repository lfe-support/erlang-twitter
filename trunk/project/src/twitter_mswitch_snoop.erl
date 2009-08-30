%% Author: Jean-Lou Dupont
%% Created: 2009-08-28
%% Description: Message Snooper listening on the local mswitch 'notif' bus. 
%%
%% @doc 
%% = MSWITCH Snooper Agent =
%%
%% == Duties ==
%% <ul>
%%  <li>Listen on 'notif' mswitch bus</li>
%%  <li>Translate 'notif' mswitch received messages to local 'notif' bus</li>
%%	<li>Translate messages only when in 'working' state (use sys.resume)</li>
%%  <li>Listen on local 'sys' mswitch for 'reload' event</li>
%%  <li>Listen on local 'sys' mswitch for 'suspend' event</li>
%%  <li>Listen on local 'sys' mswitch for 'resume' event</li>
%%  <li></li>
%% </ul>
%%
%% == Translation ==
%% The message format 'snooped' on must have the following format:
%% ```
%%	{From, notif, {Priority, Msg}}
%% '''
%%
%% Received messages (from the mswitch side) will be translated to the following
%% format when sent on the local message switch on bus 'notif':
%%
%% ```
%%	{snooper, notif, {Priority, Msg}}
%% '''
%%
%% == Mswitch subscriptions ==
%% <ul>
%%  <li>notif</li>
%% </ul>
%%
%% == Local Switch Subscriptions ==
%% The typical 'system' messages will be actioned:
%% <ul>
%%  <li>sys: reload, suspend, resume</li>
%% </ul>
%% 
%% === Format ===
%% ```
%%	{From, sys, reload}
%% '''
%%
%% ```
%%	{From, sys, suspend}  and   {From, sys, resume} 
%% '''
%%
%% == Local Switch Publications ==
%% <ul>
%%  <li>log</li>
%% </ul>
%%
%% == Configuration ==
%% None.


-module(twitter_mswitch_snoop).

-define(MSWITCH, mswitch).
-define(SERVER, snooper).
-define(SWITCH, twitter_hwswitch).
-define(MSWITCH_BUSSES, [notif]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).

%%
%% Management Functions
%%
-export([
		 start_link/1
		 ,stop/0
		,loop/0
		 ]).

%% MSWITCH RELATED
-export([
		 inbox/1
		,handle/1
		 ]).

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
	[].


%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  Management  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------
start_link(_Args) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?SERVER, Pid),
	Pid ! start,
	{ok, Pid}.

stop() ->
	try 
		?SERVER ! stop,
		ok
	catch
		_:_ ->	{error, cannot_stop}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
loop() ->
	receive
		
		%%% MANAGEMENT RELATED %%%
		start ->
			subscribe();		
		
		stop ->
			exit(normal);
	
		%%% MSWITCH RELATED %%%
		{hwswitch.bounce, _Bus, _Msg} ->
			%% @TODO log?
			ok;
		
		{mswitch, From, notif, Message} ->
			handle({mswitch, From, notif, Message});
	
		%%% LOCAL SWITCH RELATED %%%
		{hwswitch, From, Bus, Msg} ->
			handle({hwswitch, From, Bus, Msg});
	
		Other ->
			log(warning, "snoop: unexpected message: ", [Other])
	end,
	loop().


subscribe() ->
	?MSWITCH:subscribe(?MSWITCH_BUSSES).	


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  MSWITCH  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

inbox({FromNode, Server, Bus, Message}) ->
	%%io:format("inbox, bus[~p] message[~p]~n",[Bus, Message]),
	Server ! {mswitch, FromNode, Bus, Message}.


%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% @doc Translate mswitch 'notif' born messages
%%		to the internal switch format.
handle({mswitch, _From, notif, Message}) ->
	State=get_state(),
	cond_forward(State, Message);

%% Not much to do
handle({hwswitch, _From, sys, reload}) ->
	ok;

handle({hwswitch, _From, sys, suspend}) ->
	set_state(suspended);

handle({hwswitch, _From, sys, resume}) ->
	set_state(working);

handle(Other) ->
	log(warning, "Unexpected message: ", [Other]).




cond_forward(working, Msg) ->
	?SWITCH:publish(notif, Msg);

cond_forward(_, _Msg) ->
	ok.



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOGGER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%%log(Severity, Msg) ->
%%	log(Severity, Msg, []).

log(Severity, Msg, Params) ->
	?SWITCH:publish(log, {?SERVER, {Severity, Msg, Params}}).

	

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

	
	

%% Author: Jean-Lou Dupont
%% Created: 2009-08-24
%% Description: Logger with contextual policing.
%%
-module(twitter_policed_logger).
-compile(export_all).

-define(MNG,     twitter_mng).
-define(LOGGER,  twitter_log).
-define(POLICER, twitter_policer).


-export([
		 start/0, is_started/0, start_if_not/0,
		 stop/0,
		 loop/0
		 ]).
%%
%% API Functions
%%

%% @doc Initialization
%%
%% @spec init() -> void()
%%
init() ->
	?POLICER:init(),
	start_if_not().



%% @doc Log a Message with Severity and
%%      policed through Context.
%%
%% @spec log(Context, Severity, Msg) -> void()
%% where
%%	Context = atom()
%%	Severity = atom()
%%	Msg = string() | atom() | integer() | list()
%%
log(Context, Severity, Msg) ->
	try
		?MODULE ! {log, Context, Severity, Msg}
	catch
		_:_ -> 
			io:format("~p: error, not init~n",[?MODULE])
	end.
			

log(Context, Severity, Msg, Params) ->
	try
		?MODULE ! {log, Context, Severity, Msg, Params}
	catch
		_:_ ->
			io:format("~p: error, not init~n",[?MODULE])
	end.
		



%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------



start() ->
	Pid=spawn(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok, Pid}.

start_if_not() ->
	case is_started() of
		false -> start();
		_     -> started
	end.


is_started() ->
	Pid = erlang:whereis(?MODULE),
	case Pid of
		undefined -> false;
		_         -> erlang:is_process_alive(Pid)
	end.

%% We do not want the caller process
%% to crash potentially just because 
%% we are attempting to stop this one.
stop() ->
	try
		?MODULE ! stop
	catch
		_:_ -> error
	end.


loop() ->
	receive 
		stop ->
			exit(ok);
		
		{log, Ctx, Sev, Msg} ->
			?POLICER:police(Ctx, self(), 
							{pass, Sev, Msg, []}, 
							{drop, Sev, Msg, []});

		{log, Ctx, Sev, Msg, Params} ->
			?POLICER:police(Ctx, self(), 
							{pass, Sev, Msg, Params}, 
							{drop, Sev, Msg, Params});
		
		
		{drop, _Sev, _Msg, _Params} ->
			?MNG:inc_stat(log_policed_drop);
		
		{pass, Sev, Msg, Params} ->
			?LOGGER:log(Sev, Msg, Params);
		
		_ ->
			?MNG:inc_stat(policed_log_invalid_msg_received)
	end,
	loop().




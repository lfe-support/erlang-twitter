%% Author: Jean-Lou Dupont
%% Created: 2009-08-25
%% Description: Logging facility
%%
%% Configuration:
%%	LogName
%%	LogDir
%%	
-module(twitter_log2).

%% Keep this registered name in order for
%% other processes to easily interface to it.
-define(SERVER, logger).


-define(DEFAULT_DIR, "/var/log/").


%%
%% Exported Functions
%%
-export([
		 start_link/0, start_link/1
		 ]).

-export([
		 loop/0
		,handle/1
		 ]).

%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

%% @doc start_link
%%		Should be avoided
%% @spec start_link() -> {ok, Pid}
%%
start_link() ->
	run(?SERVER, ?MODULE).

%% @doc start_link
%%
%% @spec start_link(LogName) -> {ok, Pid}
%%
start_link([{logfilename, LogName}]) ->
	run(?SERVER, LogName).


%% @private
run(Server, LogName) ->
	Pid=spawn_link(?MODULE, loop, []),
	register(Server, Pid),
	Server ! {logfilename, LogName},
	{ok, Pid}.


%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------

%% @private
loop() ->
	receive
		stop   -> handle(stop);
		reload -> handle(reload);
		{logfilename, LogName} -> handle({start, LogName})
	end,
	loop().


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

handle({start, LogName}) ->
	ok;

handle(stop) ->
	exit(ok);

%% @doc Handle 'reload' event
%%
handle(reload) ->
	ok;

handle(_) ->
	ok.



%%
%% Local Functions
%%

%%
%% API Functions
%%
init() ->
	init(?DEFAULT_DIR++?DEFAULT_LOG++".log").

init(LogFileName) when length(LogFileName)==0 ->
	init(?DEFAULT_DIR++?DEFAULT_LOG++".log"); %% easily portable

%% Initializes the log file
%% - closes (or attempts) to close the current log file
%% - opens the specified log
%% - sets the process variables
%%
init(LogFileName) ->
	CurrentLog=get(log),
	
	%% ignore return code
	_Ret=?LOG:close(CurrentLog),
	
	%%io:format("Ret: ~p~n",[Ret]),

	%% opens the log...
	Params=[
		{name,    ?DEFAULT_LOG}
		%%,{linkto, none}
		,{file,   LogFileName}
		,{type,   wrap}
		,{mode,   read_write}
		,{format, external}
		,{size,   {?DEFAULT_LOG_SIZE, ?DEFAULT_LOG_FILES}}
		,{repair, true}		   
	],
	Ret2=?LOG:open(Params),
	process_open(Ret2),
	%%io:format("Log:init: completed~n"),
	Ret2.

%% @doc Records log open errors
%%
process_open({error, _Reason}) ->
	?MNG:inc_stat(?STAT_OPEN_ERROR);

process_open({ok, Log}) ->
	%%io:format("process_open: log: ~p~n", [Log]),
	put(log, Log);

process_open({repaired, Log, _}) ->
	put(log, Log);

process_open(_) ->
	?MNG:inc_stat(?STAT_OPEN_ERROR).


%% @doc Log a message with 'info' severity
%%
log(Msg) ->
	Logger=get(log),
	dolog(info, Logger, Msg, []).

%% @doc Log a message with specific severity
%%
log(Severity, Msg) ->
	Logger=get(log),
	dolog(Severity, Logger, Msg, []).


log(Severity, Msg, Params) ->
	Logger=get(log),
	dolog(Severity, Logger, Msg, Params).


dolog(_Severity, undefined, _, _Params) ->
	io:format("Logger undefined!~n"),
	?MNG:inc_stat(?STAT_LOG_ERROR);

dolog(Severity, Logger, Msg, []) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	FMsg=io_lib:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]),
	Ret=?LOG:balog(Logger, FMsg),
	record_result(Ret);


dolog(Severity, Logger, Msg, Params) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	FMsg=io_lib:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B [~s] ~s:  ~p ~p~n",[Day, Month, Year,Hour,Min,Sec, Severity, ?DEFAULT_LOG, Msg]++Params),
	Ret=?LOG:balog(Logger, FMsg),
	record_result(Ret).



record_result(ok) ->
	ok;

record_result(Result) ->
	io:format("Log error: ~p~n",[Result]),
	?MNG:inc_stat(?STAT_LOG_ERROR).


	
close() ->
	Log=get(log),
	?LOG:close(Log).



%% ----------------------             ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------             ------------------------------

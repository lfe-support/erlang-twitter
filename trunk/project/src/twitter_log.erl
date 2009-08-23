%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Logging facility
-module(twitter_log).

-define(DEFAULT_LOG, "twitter").
-define(DEFAULT_LOG_SIZE, 10*1000*1000).
-define(DEFAULT_LOG_FILES, 10).
-define(LOG, disk_log).
-define(DEFAULT_DIR, "/var/log/").
-define(MNG, twitter_mng).
-define(STAT_OPEN_ERROR, error_log_open).
-define(STAT_LOG_ERROR,  error_log_msg).


%%
%% Exported Functions
%%
-export([
		 init/0, init/1,
		 log/1,
		 close/0
		 ]).

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
	Ret2.


process_open({error, _Reason}) ->
	?MNG:inc_stat(?STAT_OPEN_ERROR);

process_open({ok, Log}) ->
	put(log, Log);

process_open({repaired, Log, _}) ->
	put(log, Log);

process_open(_) ->
	?MNG:inc_stat(?STAT_OPEN_ERROR).



log(Msg) ->
	Log=get(log),
	dolog(Log, Msg).

dolog(undefined, _) ->
	?MNG:inc_stat(?STAT_LOG_ERROR);

dolog(Log, Msg) ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_datetime(erlang:now()),
	FMsg=io_lib:format("~2B/~2B/~4B ~2B:~2.10.0B:~2.10.0B  ~s:  ~p~n",[Day, Month, Year,Hour,Min,Sec, ?DEFAULT_LOG, Msg]),
	Ret=?LOG:balog(Log, FMsg),
	record_result(Ret).

record_result(ok) ->
	ok;

record_result(_) ->
	?MNG:inc_stat(?STAT_LOG_ERROR).


	
close() ->
	Log=get(log),
	?LOG:close(Log).




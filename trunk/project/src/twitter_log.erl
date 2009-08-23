%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Logging facility
-module(twitter_log).

-define(DEFAULT_LOG, "twitter").
-define(DEFAULT_LOG_SIZE, 10*1000*1000).
-define(LOG, disk_log).
-define(DEFAULT_DIR, "/var/log").
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
	init(?DEFAULT_DIR++"/"++?DEFAULT_LOG++".log").

init(LogFileName) when length(LogFileName)==0 ->
	init(?DEFAULT_DIR++"/"++?DEFAULT_LOG++".log"); %% easily portable

%% Initializes the log file
%% - closes (or attempts) to close the current log file
%% - opens the specified log
%% - sets the process variables
%%
init(LogFileName) ->
	CurrentLog=get(log),
	
	%% ignore return code
	?LOG:close(CurrentLog),
	
	%%io:format("Filename: ~p~n",[LogFileName]),

	%% opens the log...
	Params=[
		{name,    ?MODULE}
		,{file,   LogFileName}
		,{type,   wrap}
		,{mode,   read_write}
		,{format, external}
		,{size,   ?DEFAULT_LOG_SIZE}	
		,{repair, truncate}		   
	],
	Ret=?LOG:open(Params),
	process_open(Ret),
	Ret.


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
	Ret=?LOG:balog(Log, Msg),
	record_result(Ret).

record_result(ok) ->
	ok;

record_result(_) ->
	?MNG:inc_stat(?STAT_LOG_ERROR).


	
close() ->
	Log=get(log),
	?LOG:close(Log).


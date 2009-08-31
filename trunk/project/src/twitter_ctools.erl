%% Author: Jean-Lou Dupont
%% Created: 2009-08-31
%% Description: Configuration related tools.
-module(twitter_ctools).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(CONFIG_FILENAME, ".twitter").
-define(LOG,      twitter_log).
-define(TOOLS,    twitter_tools).



load_config() ->
	load_config_file().


%% Mainly used for debugging
%% @private
load_config(Config) ->
	%?CTOOLS:put_defaults(),
	Result=extract_config(Config),
	validate_config(Config),
	put(config_state, Result),
	Result.
	


%% @doc Loads the config file and updates the local variables & config state. 
%%
%% The function loads all the Default configuration parameters first
%% and updates/complements them with the parameters on file.
%%
%% @spec load_config() -> {error, Reason} | {ok, Config}
%%
%% @private
load_config_file() ->
	case read_config() of
		{error, Reason} ->
			?LOG:log(error, "File error reading configuration file, reason: ", [Reason]),
			put(config_state, {error, Reason}),
			{error, Reason};
		
		{ok, Config} ->
			%?CTOOLS:put_defaults(),
			%Result=extract_config(Config),
			%validate_config(Config),
			%put(config_state, Result),
			%Result
			Config
	end.



get_config_file_mtime() ->
	Filename=config_filename(),
	case file:read_file_info(Filename) of
		{ok, FileInfo} ->
			FileInfo#file_info.mtime;
		_ ->
			error
	end.




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LOCAL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
get_defaults() ->
	load_defaults([], []).
	
load_defaults([], Acc) ->
	Acc;

%% @doc Load defaults
%%		Go through the modules list and
%%		queries the default values through the
%%		function 'defaults'.
%% @spec load_defaults(Modules) -> void()
%% where
%%	Modules = [atom()]
%%
load_defaults([Module|Modules], Acc) ->
	Defaults=try_load_module_defaults(Module),
	List=try_check_defaults(Defaults, []),
	FilteredList=filter_entries(List, []),
	load_defaults(Modules, Acc++[{Module, FilteredList}]).


try_load_module_defaults(Module) ->
	try
		erlang:apply(Module, defaults, [])
	catch
		_:_ ->
			?LOG:log(debug, "config: no defaults for module: ", [Module]),
			no_defaults
	end.


try_check_defaults([], []) ->
	[];

try_check_defaults([], List) ->
	List;

try_check_defaults(Defaults, List) when is_list(Defaults) ->
	[Default|Rest]=Defaults,
	Entry= check1_one_default(Default),
	E1= check2_one_default(Entry),
	E2= check3_one_default(E1),
	try_check_defaults(Defaults, List++[E2]).


check1_one_default({}) ->
	{};

check1_one_default(Default) when is_tuple(Default) ->
	try
		{Key, Level, Type, Value} = Default,
		{Key, Level, Type, Value}
	catch
		_:_ ->
			?LOG:log(debug, "config: invalid entry format: ", [Default]),
			{}
	end;

check1_one_default(Default) ->
	?LOG:log(debug, "config: invalid default entry: ", [Default]),
	{}.



check2_one_default({Key, Level, Type, Value}) when is_atom(Key), is_atom(Level), is_atom(Type) ->
	{Key, Level, Type, Value};

check2_one_default(Entry) ->
	?LOG:log(debug, "config: invalid default entry: ", [Entry]),
	{}.



check3_one_default({Key, Level, Type, Value}) when Type==string ->
	case is_list(Value) of
		true ->	{Key, Level, Type, Value};
		false -> ?LOG:log(debug, "config: expecting 'string' for Key: ", [Key]), {}
	end;
check3_one_default({Key, Level, Type, Value}) when Type==nstring ->
	case is_list(Value) of
		true ->	{Key, Level, Type, Value};
		false -> ?LOG:log(debug, "config: expecting 'nstring' for Key: ", [Key]), {}
	end;
check3_one_default({Key, Level, Type, Value}) when Type==int ->
	case is_integer(Value) of
		true ->	{Key, Level, Type, Value};
		false -> ?LOG:log(debug, "config: expecting 'int' for Key: ", [Key]), {}
	end;
check3_one_default({Key, Level, Type, Value}) when Type==float ->
	case is_float(Value) of
		true ->	{Key, Level, Type, Value};
		false -> ?LOG:log(debug, "config: expecting 'float' for Key: ", [Key]), {}
	end;
check3_one_default({Key, Level, Type, Value}) when Type==atom ->
	case is_atom(Value) of
		true ->	{Key, Level, Type, Value};
		false -> ?LOG:log(debug, "config: expecting 'atom' for Key: ", [Key]), {}
	end.



filter_entries([], Acc) ->
	Acc;

filter_entries([{}|Rest], Acc) ->
	filter_entries(Rest, Acc);
	
filter_entries([Entry|Rest], Acc) ->
	filter_entries(Rest, Acc++[Entry]).

	







%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

%% @doc Reads the configuration file
%%
%% @spec read_config() -> {error, Reason} | {ok, Terms}
%% where 
%% 	Terms = [tuple()]
%%
read_config() ->
	Filename=config_filename(),
	read_config(Filename).

read_config(error) ->
	{error, "environment 'HOME' not defined"};

read_config(Filename) ->
	case file:consult(Filename) of
		{error, Reason} ->
			{error, Reason};
		{ok, Terms} ->
			{ok, Terms}
	end.



config_filename() ->
	Home=os:getenv("HOME"),
	config_filename(Home).
	
config_filename(false) ->
	error;

config_filename(Home) ->
	Home++"/"++?CONFIG_FILENAME.

	


%% @doc Retrieves the 'min' value for Key in the Defaults
%%
%% @spec get_min(Key) -> TypedValue
%% where
%%	Key=atom()
%%	Value=atom() | list() | undefined
%%	TypedValue= {Type, Value}
%%  Type= atom()
%%	Value=atom() | list() | undefined | invalid
get_min(Key) ->
	get_special(".min", Key).
	

%% @doc Retrieves the 'max' value for Key in the Defaults
%%
%% @spec get_max(Key) -> TypedValue
%% where
%%	Key=atom()
%%	TypedValue= {Type, Value}
%%  Type= atom()
%%	Value=atom() | list() | undefined
%%
get_max(Key) ->
	get_special(".max", Key).



%% @doc Retrieves the TypedDefault for Key
%%
%%
get_special(Pattern, Key) when is_atom(Pattern) ->
	Pat=erlang:atom_to_list(Pattern),
	get_special(Pat, Key);

get_special(Pattern, Key) when is_list(Pattern) ->
	Var=erlang:atom_to_list(Key)++Pattern,
	Vara=erlang:list_to_atom(Var),
	TypedDefault=?TOOLS:kfind(Vara, ?DEFAULTS:defaults()),
	get_special(Pattern, Key, TypedDefault).


get_special(_Pattern, _Key, {}) ->
	undefined;
	

get_special(_Pattern, _Key, TypedDefaultValue) ->
	%%io:format("get_special: Key:<~p> typed: ~p~n",[Key, TypedDefaultValue]),	
	TypedDefaultValue.




has_pattern(Patterns, Key) when is_atom(Key) ->
	has_pattern(Patterns, erlang:atom_to_list(Key));

has_pattern(Patterns, Key) when is_list(Patterns) ->
	do_has_pattern(false, Patterns, Key).

do_has_pattern(true, _, _) ->
	true;

do_has_pattern(false, [], _Key) ->
	false;
	
do_has_pattern(false, Patterns, Key) ->
	[Pattern|Rest] = Patterns,
	Result=check_pattern(Pattern, Key),
	do_has_pattern(Result, Rest, Key).


check_pattern(Pattern, Key) ->
	Str=erlang:atom_to_list(Pattern),
	case string:str(Key, Str) of
		0 -> false;
		_ -> true
	end.



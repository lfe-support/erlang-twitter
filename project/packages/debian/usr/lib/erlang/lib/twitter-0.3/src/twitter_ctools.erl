%% Author: Jean-Lou Dupont
%% Created: 2009-08-31
%% Description: Configuration related tools.
-module(twitter_ctools).
-compile(export_all).

-include_lib("kernel/include/file.hrl").

-define(CONFIG_FILENAME, ".twitter").
-define(LOG,      twitter_log).
-define(TOOLS,    twitter_tools).


%% @doc Loads & validates system configuration from file
%%
%%
do_config(Modules) ->
	Defaults=get_defaults(Modules),
	%io:format("Defaults: ~p~n", [Defaults]),	
	Result=process_config(Modules, Defaults),
	case Result of
		{ok, Mtime, Config} ->
			%io:format("Config: ~p~n", [Config]),
			Merged=merge(Defaults, Config),
			{ok, Mtime, Merged};
		Other ->
			Other
	end.






%% @doc Processes configuration from file
%%		The following function loads configuration information from file:
%%		- Read raw terms
%%		- 
%%
%% @spec process_config() -> {error, Reason} | {ok, Mtime, ModuleConfig}
%%
process_config(Modules, Defaults) ->
	case load_config_file() of
		{error, Reason} ->
			% already logged
			{error, Reason};
		
		{ok, Mtime, Config} ->
			
			%io:format("Config file content: ~p~n", [Config]),
			
			%% 1) check format
			List=  do_process_config(Config, []),
			Blacklist=get_blacklist(Modules, []),
			
			%% 2) filter on blacklist
			List2= filter_on_blacklist(List, Blacklist, []),
			
			%io:format("After blacklist: ~p~n", [List2]),
			
			List3= filter_on_patterns(['.min', '.max'], List2, []),
			
			%% 3) check presence of mandatory parameters
			check_mandatory(List3, Defaults),
			
			%% 4) type check
			List4=check_type(List2, Defaults, []),
			
			List5=filter_entries(List4, []),
			
			%% 5) check limit requirements
			List6=check_limits(List5, Defaults, []),
	
			%% Remove unnecessary entries such as {}
			List7=filter_entries(List6, []),
			
			{ok, Mtime, List7}			
	end.

do_process_config([], Acc) ->
	Acc;

do_process_config(Config, Acc) when is_list(Config) ->
	[Entry|Rest] = Config,
	Fentry = filter_one(Entry),
	do_process_config(Rest, Acc++[Fentry]).


filter_one({}) -> {};
filter_one({Key, Value}) when is_atom(Key) -> {Key, Value};
filter_one(Inv) -> ?LOG:log(error, "config: invalid entry in config file: ", [Inv]).



filter_on_blacklist([], _Blacklist, Acc) -> Acc;

filter_on_blacklist([Entry|Rest], Blacklist, Acc) ->
	Key=get_key(Entry),
	Blacklisted=lists:member(Key, Blacklist),
	case Blacklisted of
		true  -> 
			?LOG:log(warning, "config: blacklisted parameter: ", [Key]),
			filter_on_blacklist(Rest, Blacklist, Acc);
		false -> filter_on_blacklist(Rest, Blacklist, Acc++[Entry])
	end.
	


filter_on_patterns(_Patterns, [], Acc) -> Acc;

filter_on_patterns(Patterns, [{Key, Value}|Rest], Acc) ->
	case has_pattern(Patterns, Key) of
		true  -> Item={};
		false -> Item={Key, Value}
	end,
	filter_on_patterns(Patterns, Rest, Acc++[Item]).



check_mandatory(_, []) ->
	finished;

%% @doc Go through the Defaults list to verify
%%		the presence of mandatory parameters
%%
check_mandatory(List, [Default|Defaults]) ->
	Key=get_key(Default),
	Level=get_level(Default),
	
	case Level of
		mandatory ->
			check_mandatory1(List, Key);
		_ ->
			ok
	end,
	check_mandatory(List, Defaults).
	

check_mandatory1(List, Key) ->
	case find_key_in_config_list(List, Key) of
		{} -> ?LOG:log(error, "config: missing mandatory parameter: ", [Key]);
		_  -> ok
	end.


check_type([], _Defaults, Acc) -> Acc;

%% @doc Go through the parameters list
%%		and check each entry type against
%%		the entries in defaults. Filter out
%%		each entry in type mismatch error.
%%
%% @spec check_type(Parameters, Defaults, Acc) -> list()
%%
check_type([ConfigEntry|Rest], Defaults, Acc) ->
	Entry=check_type_one(ConfigEntry, Defaults),
	check_type(Rest, Defaults, Acc++[Entry]).


check_type_one(ConfigEntry, Defaults) ->
	{Key, _Value}=ConfigEntry,
	Dentry=get_entry_from_defaults(Defaults, Key),
	case check_type2(Key, ConfigEntry, Dentry) of
		ok -> ConfigEntry;
		_ ->  
			case Dentry of
				no_default -> {};
				_          -> Dentry
			end
	end.


%% @doc Filters a configuration parameter
%%
check_type2(Key, _ConfigEntry, {}) ->
	?LOG:log(debug, "config: missing default entry for key: ", [Key]),
	
	%% delete entry from list of valid entries
	no_default;

check_type2(Key, {Ckey, Cvalue}, DefaultEntry) ->
	{type, TargetType}=get_type(DefaultEntry),
	{value, DefaultValue}=get_value(DefaultEntry),
	check_type3(Key, Ckey, TargetType, Cvalue, DefaultValue).

check_type3(_Key, _Ckey, atom,   Cvalue, _Dvalue) when is_atom(Cvalue)    -> ok;
check_type3(_Key, _Ckey, string, Cvalue, _Dvalue) when is_list(Cvalue)    -> ok;
check_type3(_Key, _Ckey, int,    Cvalue, _Dvalue) when is_integer(Cvalue) -> ok;
check_type3(_Key, _Ckey, float,  Cvalue, _Dvalue) when is_float(Cvalue)   -> ok;
check_type3(Key,  _Ckey, nstring,Cvalue, _Dvalue) when is_list(Cvalue) -> 
	case length(Cvalue) of
		0 -> ?LOG:log(error, "config: expecting non-zero string for key: ", [Key]),	invalid;
		_ -> ok
	end;

check_type3(Key, _Ckey, Type, _Cvalue, _Dvalue) -> 
	?LOG:log(error, "config: expecting Type for Key, {Type, Key}: ", [{Type, Key}]).
		






%% @doc Loads the config file and updates the local variables & config state. 
%%
%% The function loads all the Default configuration parameters first
%% and updates/complements them with the parameters on file.
%%
%% @spec load_config() -> {error, Reason} | {ok, Mtime, Config}
%%
%% @private
load_config_file() ->
	case read_config() of
		{error, Reason} ->
			?LOG:log(error, "config: error reading configuration file, reason: ", [Reason]),
			{error, Reason};
		
		{ok, Config} ->
			Mtime=get_config_file_mtime(),
			{ok, Mtime, Config}
	end.



get_config_file_mtime() ->
	Filename=config_filename(),
	case file:read_file_info(Filename) of
		{ok, FileInfo} ->
			FileInfo#file_info.mtime;
		{error, Reason} ->
			?LOG:log(error, "config: error getting info from file, {Filename, Reason}", [Filename, Reason]),
			unknown
	end.




%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  DEFAULTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% @doc Returns a validated list of defaults
%%		
%% @spec get_defaults(Modules) -> [ModuleDefaults]
%% where
%%	Modules=list()
%%	@type ModuleDefaults = {ModuleName, Defaults}
%%	ModuleName = atom()
%%	@type Defaults = [Entry]
%%	@type Entry={Key, Level, Type, Value}            %% <--------
%%	Key = atom()
%%	Level = atom()
%%	Type = atom()
%%	Value = atom() | list() | integer() | float()
%%
get_defaults(Modules) ->
	load_defaults(Modules, []).
	


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
	Server=get_module_server(Module),
	case Server of
		undefined ->
			?LOG:log(debug, "load_defaults: cannot access 'module server name' for module: ", [Module]),
			load_defaults(Modules, Acc);
		ServerName ->
			Defaults=try_load_module_defaults(Module),
			%io:format("load_defaults: Module[~p] Defaults[~p]~n",[Module, Defaults]),	
			List=try_check_defaults(Defaults, []),
			FilteredList=filter_entries(List, []),
			load_defaults(Modules, Acc++[{ServerName, FilteredList}])
	end.
	


try_load_module_defaults(Module) ->
	try
		erlang:apply(Module, defaults, [])
	catch
		_:_ ->
			?LOG:log(debug, "config: no defaults for module: ", [Module]),
			[]
	end.


try_check_defaults([], []) ->
	[];

try_check_defaults([], List) ->
	List;

try_check_defaults(Defaults, List) when is_list(Defaults) ->
	[Default|Rest]=Defaults,
	
	%% Entry format 'envelope' check
	Entry= check1_one_default(Default),
	
	%% Entry 'elements' format check
	E1= check2_one_default(Entry),
	
	%% type check
	E2= check3_one_default(E1),
	try_check_defaults(Rest, List++[E2]).


check1_one_default({}) -> {};

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







get_entry_from_defaults([], _Key) -> {};

%% @doc Retrieves an Entry from Key in the Defaults list
%%
%% @spec get_entry_from_defaults(Defaults, Key) -> Entry
%%
get_entry_from_defaults([ModuleEntries|Modules], Key) ->
	{_ModuleName, Entries}=ModuleEntries,
	%io:format("get_entry_from_defaults: Mod[~p] Key[~p] Entries[~p]~n",[ModuleName, Key, Entries]),
	Entry=?TOOLS:kfind(Key, Entries),
	case Entry of
		{} ->
			get_entry_from_defaults(Modules, Key);
		Entry ->
			Entry
	end.

	


%% @doc Retrieves a module's defaults
%%
%% @spec get_module_defaults(Entries, Module) -> list()
%%
get_module_defaults(Entries, Module) ->
	?TOOLS:kfind(Module, Entries).



%% @doc Retrieves a module's blacklist entries
get_module_blacklist(Module) ->
	try
		erlang:apply(Module, blacklist, [])
	catch
		_:_ -> []
	end.


%% @doc Retrieves all the blacklisted parameters
%%		from all modules
%% @spec get_black(Modules, Acc) -> list()
%%
get_blacklist([], Acc) ->
	Acc;

get_blacklist([Module|Modules], Acc) ->
	List=get_module_blacklist(Module),
	get_blacklist(Modules, Acc++List).

	


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

	

%% @doc Finds a specific Key from the tuple list
%%
find_key_in_config_list(List, Key) ->
	?TOOLS:kfind(Key, List).





%% @doc Retrieves the 'min' value for Key in the Defaults
%%
%% @spec get_min(Key) -> tuple() | {}
%% where
%%	Key=atom()
get_min(List, Key) ->
	get_special(List, ".min", Key).
	

%% @doc Retrieves the 'max' value for Key in the Defaults
%%
%% @spec get_max(Key) -> tuple() | {}
%% where
%%	Key=atom()

%%
get_max(List, Key) ->
	get_special(List, ".max", Key).



%% @doc Retrieves the complete tuple matching Key
%%
%% @spec get_special(List, Pattern, Key) -> tuple() | {}
%% where
%%	List=[tuple()]
%%	Pattern=atom()
%%	Key=atom()
get_special(List, Pattern, Key) when is_atom(Pattern) ->
	Pat=erlang:atom_to_list(Pattern),
	get_special(List, Pat, Key);

get_special(List, Pattern, Key) when is_list(Pattern) ->
	%%io:format("get_special: Key[~p] List[~p]~n", [Key, List]),
	Var=erlang:atom_to_list(Key)++Pattern,
	Vara=erlang:list_to_atom(Var),
	?TOOLS:kfind(Vara, List).




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

	


%% @doc Retrieves the Server associated with Module
%%
%% @spec get_module_server(Module) -> undefined | atom()
%%
get_module_server(Module) ->
	try
		erlang:apply(Module, get_server, [])
	catch
		_:_ -> undefined
	end.



%% @doc Merge the parameters from the Defaults
%%		with ones from the configuration file
%%
%%		It is assumed that the 'Config' list has
%%		been validated.
%%
%%		Config   =[{ModuleName, [{ParamName, Value}]} ]
%%		Defaults =[{ModuleName.ParamName, Level, Type, Value}]
%%
%%		Result= [{ModuleName.Param, Value}]
%%
%% @spec merge(Defaults, Config) -> [tuple()]
%%
merge(Defaults, Config) ->
	%io:format("merge: defaults<~p>~n", [Defaults]),
	%io:format("merge: config<~p>~n", [Config]),
	
	% start with all the Defaults in list
	do_merge(Config, Defaults).


do_merge([], Acc) ->
	Acc;

do_merge([ConfigEntry|ConfigEntries], Acc) ->
	%io:format("do_merge: ce[~p]~n", [ConfigEntry]),
	
	try
		{ParamName, Value}=ConfigEntry,
		%io:format("do_merge: param[~p] value[~p]~n",[ParamName, Value]),
		ModuleName=extract_module_name(ParamName),
		%io:format("do_merge: moduleName[~p]~n",[ModuleName]),
		NewList=insert_entry(ModuleName, ParamName, Value, Acc),
		do_merge(ConfigEntries, NewList)
	catch
		_:_ ->
			?LOG:log(error, "config: error whilst merging defaults+config"),
			{error, merging}
	end;

do_merge(Other, _) ->
	?LOG:log(critial, "config: exception whilst merging: ", [Other]).


insert_entry(ModuleName, ParamName, Value, List) ->
	NewList=?TOOLS:rem_from_tuple_list(List, ModuleName, ParamName),
	?TOOLS:add_to_tuple_list(NewList, ModuleName, {ParamName, Value}).





is_for_module(ModuleName, Key) ->
	HeadKey=?TOOLS:extract_head(Key),
	is_for_module2(ModuleName, HeadKey).

is_for_module2(X, X) -> true;
is_for_module2(_, _) -> false.


extract_module_name(Key) ->
	?TOOLS:extract_head(Key).



get_module_entries(ModuleName, Config) ->
	get_module_entries(ModuleName, Config, []).


get_module_entries(_ModuleName, [], Acc) -> Acc;
get_module_entries(ModuleName, [Entry|Rest], Acc) ->
	{ParamName, Value}=Entry,
	case is_for_module(ModuleName, ParamName) of
		true  -> get_module_entries(ModuleName, Rest, Acc++[{ParamName, Value}]);
		false -> get_module_entries(ModuleName, Rest, Acc)
	end.


put_config(Version, ConfigData) ->
	put(config.version, Version),
	put_config(ConfigData).

put_config([]) ->
	ok;
	
put_config(ConfigData) when is_list(ConfigData) ->
	[Entry|Entries] = ConfigData,
	try
		put_config_one(Entry)
	catch 
		_:_ ->
			?LOG:log(critical, "put_config: error with Entry: ", [Entry])
	end,
	put_config(Entries);
	
put_config(_) ->
	{error, invalid_param}.

%% For entries from Defaults
put_config_one({Param, _Level, _Type, Value}) ->
	put(Param, Value);

%% For entries from Configuration File
put_config_one({Param, Value}) ->
	put(Param, Value).


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  BOILER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  PLATE   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  FOR     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  MODULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


%% NOTE the process dictionary variable 'config.version'
%%
do_publish_config_version(Switch, Server) ->
	Version=get(config.version),
	erlang:apply(Switch, publish, [sys, {mod.config, Server, Version}]).

do_config(_Switch, _Server, undefined) -> ok;
do_config(Switch, Server, VersionInForce) ->
	Version=get(config.version),
	maybe_ask_for_config(Switch, Server, VersionInForce, Version).	

maybe_ask_for_config(_,_, X, X) -> ok;
maybe_ask_for_config(Switch, Server, _, _) -> do_publish_config_version(Switch, Server).




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  LIMITS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------



check_limits([], _Defaults, Acc) ->
	Acc;

check_limits([ConfigEntry|Entries], Defaults, Acc) ->
	Entry=check_limit_one(ConfigEntry, Defaults),
	check_limits(Entries, Defaults, Acc++[Entry]).



check_limit_one({Key, Value}, Defaults) ->
	Default=get_var_in_defaults(Key, Defaults),
	case Default of 
		undefined ->
			?LOG:log(debug, "config: missing default for key: ", [Key]);
		{{_, Entries}, {_, _Level, int, _Default}} ->
			MinResult=get_min(Entries, Key),
			MaxResult=get_max(Entries, Key),
			%io:format("check_limit_one2: key[~p] val[~p] min[~p] max[~p]~n", [Key, Value, MinResult, MaxResult]),
			check_limit_one2({Key, Value}, MinResult, MaxResult);
		{{_, Entries}, {_, _Level, float, _Default}} ->
			MinResult=get_min(Entries, Key),
			MaxResult=get_max(Entries, Key),
			%io:format("check_limit_one2: key[~p] val[~p] min[~p] max[~p]~n", [Key, Value, MinResult, MaxResult]),
			check_limit_one2({Key, Value}, MinResult, MaxResult);
		_ ->
			type_without_limit_check,
			{Key, Value}
	end;			
	

%% shouldn't get here...
check_limit_one(_, _Defaults) ->
	ok.

check_limit_one2({Key, Value}, {}, {}) ->
	?LOG:log(debug, "config: missing 'min' default for key: ", [Key]), 
	?LOG:log(debug, "config: missing 'max' default for key: ", [Key]),
	{Key, Value};

check_limit_one2({Key, Value}, Result, {}) ->
	Limit=get_value(Result),
	?LOG:log(debug, "config: missing 'max' default for key: ", [Key]),
	check_limit(min, {Key, Value}, Limit);

check_limit_one2({Key, Value}, {}, Result) ->
	Limit=get_value(Result),
	?LOG:log(debug, "config: missing 'min' default for key: ", [Key]),
	check_limit(max, {Key, Value}, Limit);


check_limit_one2({Key, Value}, MinResult, MaxResult) ->
	MinV=get_value(MinResult),
	MaxV=get_value(MaxResult),
	%io:format("check_limit_one2: minv[~p] maxv[~p]~n", [MinV, MaxV]),
	{K1, V1}=check_limit(min, {Key, Value}, MinV),
	check_limit(max, {K1, V1}, MaxV).




check_limit(max, {Key, Value}, {_, Limit}) when (is_integer(Value) or is_float(Value)) 
  											and (is_integer(Limit) or is_float(Limit)) ->
	case Value > Limit of
		true ->	?LOG:log(error, "config: value 'too high' {Key, Value, MaxValue} ", [Key, Value, Limit]), 
				{Key, Limit};
		false->	{Key, Value}
	end;


check_limit(min, {Key, Value}, {_, Limit}) when (is_integer(Value) or is_float(Value)) 
  											and (is_integer(Limit) or is_float(Limit)) ->
	case Value < Limit of
		true ->	?LOG:log(error, "config: value 'too low' {Key, Value, MinValue} ", [Key, Value, Limit]), 
				{Key, Limit};
		false->	{Key, Value}
	end;

check_limit(_, {Key, _Value}, Limit) ->
	?LOG:log(error, "config: invalid default value for {Key, DefaultValue}: ", [[Key, Limit]]),
	{Key, Limit}.



%% @doc Retrieves a specific entry in the Defaults list
%%
%% @spec get_var_in_defaults(VarName, Defaults) -> undefined | {[ModuleEntries], Entry}
%% where
%%	@type ModuleEntries=[tuple()]
%%	@type Entry=tuple()
%%
get_var_in_defaults(VarName, Defaults) ->
	ModuleName=?TOOLS:extract_head(VarName),
	ModuleEntries=?TOOLS:kfind(ModuleName, Defaults),
	case ModuleEntries of
		{} -> undefined;
		{_, Entries} ->
			{ModuleEntries, ?TOOLS:kfind(VarName, Entries)}
	end.



filter_entries([], Acc)           -> Acc;
filter_entries([[]|Rest], Acc)    -> filter_entries(Rest, Acc);
filter_entries([{}|Rest], Acc)    -> filter_entries(Rest, Acc);
filter_entries([Entry|Rest], Acc) -> filter_entries(Rest, Acc++[Entry]).


%% @doc Retrieves the Key field from an Entry (defaults format)
%% 
%% @spec get_key(Entry) -> Key
%% where 
%%	Entry = {Key, Level, Type, Value}
%%	Key = atom()
get_key({Key, _Level, _Type, _Value}) -> {key, Key};
get_key(_) -> error.

get_level({_Key, Level, _Type, _Value}) -> {level, Level};
get_level(_) ->	error.

get_type({_Key, _Level, Type, _Value}) -> {type, Type};
get_type(_) -> error.

get_value({_Key, _Level, _Type, Value}) -> {value, Value};
get_value(_) -> error.



	
%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TEST  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

test() ->
	Modules=[twitter_app, twitter_log, twitter],
	do_config(Modules).	

%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: Management functions
%%
%% @doc
%%  ==Required Parameters==
%%  <ul>
%%   <li>user</li>
%%   <li>pass</li>
%%  </ul>
%%
%% Parameters retrieved from DEFAULTS are *typed* whereas
%% the ones retrieved from the process dictionary aren't.
%%
-module(twitter_mng).
-compile(export_all).

-export([
		 get_param/2, put_param/2
		 ]).

%%
%% Macros
%%
-define(CONFIG_FILENAME, ".twitter").
-define(CTOOLS,          twitter_config_tools).
-define(TOOLS,           twitter_tools).
-define(DEFAULTS,        twitter_defaults).
-define(LOG,             twitter_log).



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CONFIG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

load_config() ->
	load_config_file().


%% Mainly used for debugging
%% @private
load_config(Config) ->
	?CTOOLS:put_defaults(),
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
			?CTOOLS:put_defaults(),
			Result=extract_config(Config),
			validate_config(Config),
			put(config_state, Result),
			Result
	end.

%do_load_config

%% @doc Debug only (probably)
%%
%% @private
extract_config() ->
	{ok, Config} = read_config(),
	extract_config(Config).


%% Extracts the configuration parameters from the Config terms.
%%
%% The 'terms' should have already been 'sanitized'.
%% @spec extract_config(Config) -> {ok, Config} | {error, Reason}
%%
%% Config = list()
%%
%% @private
extract_config(Config) ->
	try
		%% insert config parameters in the process dictionary
		extract_params(Config),
		ok
	catch
		_:_ ->
			?LOG:log(error, "Parameters extraction from configuration file.")
	end.




extract_params([]) ->
	ok;

%% @doc Extracts the {param, ...} parameters
%%      from the config list and 'puts'
%%      them in the process dictionary whilst
%%      respecting any 'blacklisted' elements.
%%
extract_params(Config) when is_list(Config)->
	[Param|Rest] = Config,
	%%io:format("extract_params: param: ~p~n",[Param]),
	try
		{{param, ParamName}, {Type, Value}} = Param,
		%%io:format("extract_params: param: ~p type: ~p ~n",[ParamName, Type]),
		safe_put_param(ParamName, Type, Value)
	catch
		_X:_Y ->
			%%io:format("X[~p] Y[~p]~n",[X,Y]),
			?LOG:log(error, "Extraction: Invalid parameter: ", [Param]),
			{error, {invalid_param, Param}}
	end,
	extract_params(Rest).



safe_put_param(Name, atom, Value) ->
	do_safe_put_param(Name, Value);

safe_put_param(Name, int, Value) ->
	do_safe_put_param(Name, Value);

safe_put_param(Name, string, Value) ->
	do_safe_put_param(Name, Value);

safe_put_param(Name, nstring, Value) ->
	do_safe_put_param(Name, Value);

safe_put_param(Name, Type, Value) ->
	?LOG:log(error, "Invalid type: {Name, Type, Value}", [Name, Type, Value]).



do_safe_put_param(Name, Value) ->
	Blacklisted=lists:member(Name, ?DEFAULTS:blacklist()),
	put_param(Blacklisted, Name, Value).
	

put_param(true, Name, _Value) ->
	?LOG:log(warning, "Attempt to set a blacklisted key: ",[Name]),
	{error, {parameter_blacklisted, Name}};

put_param(false, Name, Value) ->
	put({param, Name}, Value).


%% @doc Put a parameter in the process dictionary.
%%		Value is *not* typed.
%%
%% @spec put_param(Name, Value) -> void()
%%	where
%%		Name  = atom()
%%		Value = atom() | string() | integer()
put_param(Name, Value) ->
	put_param(false, Name, Value).



get_param(Name, Default) ->
	?TOOLS:getvar({param, Name}, Default).




%% @doc Reads the configuration file
%%
%% @spec read_config() -> {error, Reason} | {ok, Terms}
%% 
%% Terms = [tuple()]
%% Reason = atom()
%%
%% @private
read_config() ->
	Home=os:getenv("HOME"),
	read_config(Home, ?CONFIG_FILENAME).

read_config(false, _Filename) ->
	{error, home_not_defined};

read_config(Root, Filename) ->
	case file:consult(Root++"/"++Filename) of
		{error, Reason} ->
			{error, Reason};
		
		{ok, Terms} ->
			process_config(Terms)
	end.


%% Process config file looking for
%% missing parameters. The only required parameters
%% are, at the moment, the following:
%%  1) user
%%  2) pass
%%
%% @spec process_config(Terms) -> {error, Reason} | {ok, Terms}
%%
%% @private
process_config([]) ->
	{error, invalid_config_file};

process_config(Terms) ->
	Ufound=lists:keymember({param, user}, 1, Terms),
	Pfound=lists:keymember({param, pass}, 1, Terms),
	process_config(Terms, Ufound, Pfound).

process_config(_Terms, false, false) ->	{error, {missing_username, missing_password}};
process_config(_Terms, true,  false) ->	{error, missing_password};
process_config(_Terms, false, true)  ->	{error, missing_username};
process_config(Terms,  true,  true)  ->	{ok, Terms}.




validate_config([]) ->
	ok;

%% @doc Go through all the configuration parameters & validate
%%		The configuration parameters are validated 1 by 1
%%		by looking up the DEFAULTS for type information.
%%
%% @spec validate_config(Config) -> void()
%% Config = [tuple()]
%%
validate_config(Config) when is_list(Config) ->
	[Entry|Rest] = Config,
	validate_config(Entry),
	validate_config(Rest);

validate_config(Entry) ->
	try
		{{param, Key}, {_Type, _Value}} = Entry, 
		CurrentValue    = get_param(Key, undefined),
		{_, ValidTypedValue}= ?CTOOLS:get_default(Key),
		validate_key(Key, CurrentValue, ValidTypedValue)
	catch
		_:_ ->
			?LOG:log(info, "Skipping entry: ", [Entry])
	end.


validate_key(_Key, _CurrentValue, {atom, _}) ->
	ok;

validate_key(Key, CurrentValue, {int, _}) ->
	%%io:format("validate_key: key[~p] CurrentValue[~p]~n",[Key, CurrentValue]),
	case erlang:is_integer(CurrentValue) of
		true  ->
			Result=?CTOOLS:validate_param_limit(Key, CurrentValue),
			validate_int(Key, Result);
		
		false ->
			?LOG:log(error, "Expecting 'int' type for key: ", [Key])
	end;


validate_key(Key, CurrentValue, {string, _}) ->
	case erlang:is_list(CurrentValue) of
		true  -> ok;
		false -> ?LOG:log(error, "Expecting 'string' type for key: ", [Key])
	end;

validate_key(Key, CurrentValue, {nstring, _}) ->
	case erlang:is_list(CurrentValue) of
		true  ->
			Size =?TOOLS:vsize(CurrentValue),
			case Size of
				undefined -> ?LOG:log(error, "Expecting string for key:",[Key]);
				0         -> ?LOG:log(error, "Expecting non-empty string for key:",[Key]);
				_         -> ok
			end;
			
		false ->
			?LOG:log(error, "Expecting 'string' type for key: ", [Key])
	end;


validate_key(Key, CurrentValue, TypedDefaultValue) ->
	io:format("validate_key: Key[~p] Value[~p] TypedDefault[~p]~n",[Key, CurrentValue, TypedDefaultValue]),
	?LOG:log(error, "Invalid key: ", [Key]).




%% @doc Verifies validity of {Key, Value}
%% 		and attempts to put 'min' value iff
%%		validation error occurs.
%%
validate_int(___, ok)                     -> ok;
validate_int(___, no_validation_possible) -> ok;
validate_int(Key, undefined) -> 
	?LOG:log(warning, "Default not defined for key: ", [Key]),
	ok;

validate_int(_Key, invalid_defaults) ->
	%% nothing we can really do here...
	%% the error was already logged upstream
	ok;

validate_int(Key, _) ->
	Default=?CTOOLS:get_min(Key),
	validate_store_min(Key, Default).

validate_store_min(Key, undefined) ->
	?LOG:log(warning, "Undefined 'min' value for key: ", [Key]);

validate_store_min(Key, {_Type, Default}) ->
	put_param(Key, Default);

validate_store_min(Key, _) ->
	?LOG:log(error, "Default 'min' value in error for key: ",[Key]).



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% PARAMS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------


%% Returns all the {param, ...} parameters
%% from the process dictionary.
%%
get_dicparams() ->
	List=get(),
	get_dicparams(List).

get_dicparams([]) ->
	[];

get_dicparams(List) when is_list(List) ->
	get_dicclass(List, param).

	
%% Retrieves a 'class' of keys from the
%% process dictionary.
%%
get_dicclass(Class) ->
	get_dicclass(get(), Class).

get_dicclass(List, Class) when is_atom(Class)->
	Fun = fun(Elem) ->
			case Elem of
				{{Class, _Name}, _Value} -> true;
				_ -> false
			end
		end,
	lists:filter(Fun, List);

get_dicclass(_,_) ->
	{error, invalid_class}.


%% @doc Retrieves & validates the Value associated with Key
%% 		If the Value is an integer, it is validated iff
%%		a 'min' and/or 'max' values are found in the defaults.
%%		
%% @spec get_validated(Key) -> Value
%% where
%%	Key = atom()
%%	Value = atom() | list()
%%
%%get_validated(Class, Key) ->
%%	ok.




%% ----------------------       ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% STATS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------       ------------------------------

get_stats() ->
	get_dicclass(stat).


%% @doc Increment a statistics counter Stat by 1
%%
%% @spec inc_stat(Stat) -> {Stat, NewCount}
%% where
%%	Stat = atom()
%%	NewCount = integer()
%%
inc_stat(Stat) ->
	inc_stat(Stat, 1).


%% @doc Increments an internal 'statistics' variable 'Stat' by 'Count'
%%      and returns the new count.
inc_stat(Stat, Count) when is_integer(Count) ->
	Value=?TOOLS:getvar({stat, Stat}, 0),
	New=Value+Count,
	put({stat, Stat}, New),
	{Stat, New};

inc_stat(_,_) ->
	{error, invalid_count}.


	
get_stat(Stat) when is_atom(Stat)->
	?TOOLS:getvar({stat, Stat}, 0);

get_stat(_) ->
	{error, invalid_name}.

put_stat(Stat, Value) when is_atom(Stat) ->
	put({stat, Stat}, Value);

put_stat(_, _) ->
	{error, invalid_name}.



%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------
test() ->
	erase(),
	?LOG:init(),
	Data= [
		    {{param, user},       {nstring, "jldupont"}}
		   ,{{param, pass},       {nstring, "some_password"}}
		   ,{{param, threshold1}, {int, 10}}
		   ,{{param, threshold2}, 10}		  
		   ],
	load_config(Data).


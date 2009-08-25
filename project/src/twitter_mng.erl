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
-module(twitter_mng).
-compile(export_all).

-export([
		 get_param/2, put_param/2
		 ]).

%%
%% Macros
%%
-define(CONFIG_FILENAME, ".twitter").
-define(TOOLS, twitter_tools).
-define(DEFAULTS, twitter_defaults).



%% ----------------------        ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% CONFIG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------        ------------------------------

	   

%% @doc Loads the config file and updates the local variables & config state. 
%%
%% The function loads all the Default configuration parameters first
%% and updates/complements them with the parameters on file.
%%
%% @spec load_config() -> {error, Reason} | {ok, Config}
%%
%% @private
load_config() ->
	case read_config() of
		{error, Reason} ->
			error_logger:error_msg("twitter: error with configuration file [~p]~n", [Reason]),
			put(config_state, {error, Reason}),
			{error, Reason};
		
		{ok, Config} ->
			?DEFAULTS:put_defaults(),
			Result=extract_config(Config),
			validate_config(Config),
			put(config_state, Result),
			Result
	end.

%% @doc Debug only (probably)
%%
%% @private
extract_config() ->
	{ok, Config} = read_config(),
	extract_config(Config).


%% Extracts the configuration parameters
%% from the Config terms.
%%
%% The 'terms' should have already been 'sanitized'.
%% @spec extract_config(Config) -> {ok, Config} | {error, Reason}
%%
%% Config = list()
%%
%% @private
extract_config(Config) ->
	try
		{user, User}=?TOOLS:kfind(user, Config),
		{pass, Pass}=?TOOLS:kfind(pass, Config),
		put(user, User),
		put(pass, Pass),
		
		%% insert config parameters in the process dictionary
		extract_params(Config),
		ok
	catch
		X:Y ->
			%% @TODO use the defined logger!
			error_logger:error_msg("twitter: extract_config [~p:~p]~n", [X,Y]),
			{error, {extracting_username, extracting_password}}
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
	try
		{param, ParamName, Value} = Param,
		safe_put_param(ParamName, Value)
	catch
		%% @TODO log errors!
		_:_ ->
			{error, {invalid_param, Param}}
	end,
	extract_params(Rest).


safe_put_param(Name, Value) ->
	Blacklisted=lists:member(Name, ?DEFAULTS:getblacklist()),
	put_param(Blacklisted, Name, Value).
	



put_param(Name, Value) ->
	put_param(false, Name, Value).

put_param(true, Name, _Value) ->
	{error, {parameter_blacklisted, Name}};

put_param(false, Name, Value) ->
	put({param, Name}, Value).



get_param(Name, Default) ->
	?TOOLS:getvar({param,Name}, Default).




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
	Ufound=lists:keymember(user, 1, Terms),
	Pfound=lists:keymember(pass, 1, Terms),
	process_config(Terms, Ufound, Pfound).

process_config(_Terms, false, false) ->	{error, {missing_username, missing_password}};
process_config(_Terms, true,  false) ->	{error, missing_password};
process_config(_Terms, false, true)  ->	{error, missing_username};
process_config(Terms,  true,  true)  ->	{ok, Terms}.



validate_config([]) ->
	ok;

%% @doc Go through all the configuration parameters & validate
%%
%% @spec validate_config(KeyList) -> void()
%% KeyList = list()
%%
validate_config(KeyList) when is_list(KeyList) ->
	[Key|Rest] = KeyList,
	validate_config(Key),
	validate_config(Rest);

validate_config(Key) ->
	
	%% unlikely to get an 'undefined' since
	%% we should be parsing a valid Key !
	Value=get_param(Key, undefined),
	
	Result=?DEFAULTS:validate_param_limit(Key, Value),
	validate_config(Key, Result).

validate_config(_Key, ok) ->
	ok;

%% Traps 'invalid' atom amongst others of course...
%%
validate_config(Key, _) ->
	Default=?DEFAULTS:get_min(Key, undefined),
	validate_store_min(Key, Default).

%% @TODO generate log error
validate_store_min(_Key, undefined) ->
	ok;

validate_store_min(Key, Default) ->
	put_param(Key, Default).



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


inc_stat(Stat) ->
	inc_stat(Stat, 1).


%% @doc Increments an internal 'statistics' variable 'Stat' by 'Count'
%%      and returns the new count.
inc_stat(Stat, Count) when is_integer(Count) ->
	Value=?TOOLS:getvar({stat, Stat}, 0),
	New=Value+Count,
	put({stat, Stat}, New),
	New;

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




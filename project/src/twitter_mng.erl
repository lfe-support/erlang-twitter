%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: Management functions
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

%% Blacklist of parameters that cannot be
%% changed through the configuration file.
-define(PARAMS_BLACKLIST, [user, pass]).


%% Loads the config file and
%% updates the local variables &
%% config state.
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
			Result=extract_config(Config),
			put(config_state, Result),
			Result
	end.

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
		{ok, Config}
	catch
		X:Y ->
			%% just in case...
			error_logger:error_msg("twitter: extract_config [~p:~p]~n", [X,Y]),
			{error, {extracting_username, extracting_password}}
	end.


%% Debug only (probably)
extract_config() ->
	{ok, Config} = read_config(),
	extract_config(Config).



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
		_:_ ->
			{error, {invalid_param, Param}}
	end,
	extract_params(Rest).


safe_put_param(Name, Value) ->
	Blacklisted=lists:member(Name, ?PARAMS_BLACKLIST),
	put_param(Blacklisted, Name, Value).
	

put_param(Name, Value) ->
	put_param(false, Name, Value).

put_param(true, Name, _Value) ->
	{error, {parameter_blacklisted, Name}};

put_param(false, Name, Value) ->
	put({param, Name}, Value).

get_param(Name, Default) ->
	?TOOLS:getvar({param,Name}, Default).


%% Reads the configuration file
%%
%% @spec read_config() -> {error, Reason} | {ok, Terms}
%% 
%% Terms = list()
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
%% missing parameters
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

process_config(_Terms, false, false) ->
	{error, {missing_username, missing_password}};

process_config(_Terms, true, false) ->
	{error, missing_password};

process_config(_Terms, false, true) ->
	{error, missing_username};

process_config(Terms, true, true) ->
	{ok, Terms}.


%% Returns all the {param, ...} parameters
%% from the process dictionary.
%%
get_dicparams() ->
	List=get(),
	get_dicparams(List).

get_dicparams([]) ->
	[];

get_dicparams(List) when is_list(List) ->
	Fun = fun(Elem) ->
				case Elem of
					{{param, _Name}, _Value} -> true;
					_ -> false
				end
	end,
	lists:filter(Fun, List).

						
	
	




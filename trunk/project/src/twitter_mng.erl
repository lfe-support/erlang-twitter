%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: Management functions
-module(twitter_mng).
-compile(export_all).

%%
%% Macros
%%
-define(CONFIG_FILENAME, ".twitter").
-define(TOOLS, twitter_tools).


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
%% Config = [Term()]
%%
%% @private
extract_config(Config) ->
	try
		{user, User}=?TOOLS:kfind(user, Config),
		{pass, Pass}=?TOOLS:kfind(pass, Config),
		put(user, User),
		put(pass, Pass),
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







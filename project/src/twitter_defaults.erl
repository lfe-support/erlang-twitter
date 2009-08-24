%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Default configuration parameters
%%
-module(twitter_defaults).
-compile(export_all).

-define(TOOLS, twitter_tools).

%% @doc Returns the parameters blacklist
%%
%%     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% >>  Blacklist of parameters that cannot be   <<
%% >>  changed through the configuration file   <<
%% >>  {param, Key, Value} pattern.             <<
%%     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%
%% @spec blacklist() -> list()
blacklist() ->
[
	user
	,pass

	%% Obviously, we do not want the user 
	%% to change the permissible limits...
	,refresh_mswitch_min
	,refresh_mswitch_max

	%% List of policers: provided just for debugging
	,policers
].



defaults() ->
[
	{refresh_limit_status, 5*60*1000}
	,{threshold1,          25}

	,{refresh_mswitch,     10*1000}
	,{refresh_mswitch_min, 10*1000}
	,{refresh_mswitch_max, 60*1000}

	%% MSWITCH related
	,{log_mswitch_error_minute,      1}
	,{log_mswitch_error_minute_min,  1}
	,{log_mswitch_error_minute_max,  2}

	,{log_mswitch_error_day,         2}
	,{log_mswitch_error_day_min,     1}
	,{log_mswitch_error_day_max,     6}
].


descriptions() ->
[
	{refresh_limit_status, 
	  	"Refresh interval (in ms) for querying Twitter about the rate_limit_status of the user."}
	,{threshold1,
	  	"Threshold at which point updates of lower importance than 1 (e.g. >1) will be discarded. "
		  ++"This threshold corresponds to _remaining_ amount of API calls left for the user."}
	,{refresh_mswitch,
	  	"Refresh interval (in ms) for synching with the mswitch."}
	,{refresh_mswitch_min,
	  	"Minimum refresh interval (in ms) for synching with the mswitch"}
	,{refresh_mswitch_max,
	  	"Maximum refresh interval (in ms) for synching with the mswitch"}

].


policers() ->
[
 
].

%% @doc Retrieves the configuration for the policers
%%		whilst respecting the configuration parameters.
%%
get_policers() ->
	ok.



%% @doc Retrieves the default Value for Var
%%
%% @spec get_default(Key) -> {Key, Value} | {}
%% where
%%		Key=atom()
%%      Value=atom() | list()
%%
get_default(Key) ->
	Defaults=defaults(),
	?TOOLS:kfind(Key, Defaults).


%% @doc Puts all the default values in the process dictionary.
%%		All Keys are stored using the pattern {param, Key}.
%%
%% @spec put_defaults() -> void()
%%
put_defaults() ->
	Defaults=defaults(),
	put_defaults(Defaults).

put_defaults([]) ->
	ok;

put_defaults(Defaults) ->
	[Default|Rest]=Defaults,
	{Key, Value}=Default,
	put({param, Key}, Value),
	put_defaults(Rest).



%% @doc Validates a parameter against declared min/max limits.  
%%		If a parameter does not have declared limits, 'ok' is returned.
%%
%% Only integer values are validated.
%%
%% @spec validate_param_limit(Key, Value) -> ok | too_low | too_high | invalid
%%
validate_param_limit(Key, Value) when is_integer(Value) ->
	%% builds a min & max atom for querying the table
	Min=erlang:atom_to_list(Key)++"_min",
	Max=erlang:atom_to_list(Key)++"_max",
	Mina=erlang:list_to_atom(Min),
	Maxa=erlang:list_to_atom(Max),
	Pmin=?TOOLS:kfind(Mina, defaults()),
	Pmax=?TOOLS:kfind(Maxa, defaults()),
	validate_limit(Value, Pmin, Pmax);

%% We only validate integers at the moment...
validate_param_limit(_,_) ->
	ok.


%% No limits
validate_limit(_Value, {}, {}) ->
	ok;

%% Just lower limit
validate_limit(Value, {_, Min}, {}) ->
	cmp(min, Value, Min);

%% Just upper limit
validate_limit(Value, {}, {_, Max}) ->
	cmp(max, Value, Max);

%% Both limits
validate_limit(Value, {_,Min}, {_,Max}) ->
	R1=cmp(min, Value, Min),
	R2=cmp(max, Value, Max),
	resolve_cmp(R1, R2);

%% CATCH-ALL
validate_limit(_, _, _) ->
	ok.


resolve_cmp(ok, ok) ->	ok;
resolve_cmp(ok, R2) ->	R2;
resolve_cmp(R1, ok) ->	R1.



cmp(min, Value, Target) when is_integer(Value), is_integer(Target) ->
	case Value > Target of
		false -> too_low;
		_     -> ok
	end;

cmp(max, Value, Target) when is_integer(Value), is_integer(Target) ->
	case Value > Target of
		false -> ok;
		_     -> too_high
	end;

cmp(_, _, _) ->
	invalid.



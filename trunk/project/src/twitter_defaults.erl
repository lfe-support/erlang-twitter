%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Default configuration parameters
%%
%% @TODO When changing defaults, a quick check should be made.
%%
%%
-module(twitter_defaults).
-compile(export_all).

-define(LOG,   twitter_policed_logger).
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
	%% Obviously, we do not want the user 
	%% to change the permissible limits...
	 refresh_mswitch_min
	,refresh_mswitch_max

	%% List of policers: provided just for debugging
	,policers

	%% Policer to Bucket Name associations
	,'policer.mswitch_error.bucket1'
	,'policer.mswitch_error.bucket2'

	%% Policer intervals
	,'bucket.bucket1.interval'
	,'bucket.bucket2.interval'
	
].



defaults() ->
[
 	{user, {nstring, ""}},
	{pass, {nstring, ""}},
	
	{refresh_limit_status,         {int, 5*60*1000}}
	,{'refresh_limit_status.min',  {int, 5*60*1000}}

	,{threshold1,          {int, 25}}
	,{'threshold1.min',    {int, 25}}

	,{refresh_mswitch,     {int, 10*1000}}
	,{refresh_mswitch.min, {int, 10*1000}}
	,{refresh_mswitch.max, {int, 60*1000}}

	%%%%%%%%% POLICERS %%%%%%%%%%%%%%%%%%%%%%%%%

	%% MSWITCH related
	,{'policer.mswitch_error.bucket1', {atom, bucket1}}
	,{'policer.mswitch_error.bucket2', {atom, bucket2}}
	
	,{'bucket.bucket1.tokens',       {int, 1}}
	,{'bucket.bucket1.tokens.min',   {int, 1}}
	,{'bucket.bucket1.tokens.max',   {int, 2}}
	,{'bucket.bucket1.interval',	 {int, 60*1000}}
	,{'bucket.bucket1.interval.min', {int, 60*1000}}

	,{'bucket.bucket2.tokens',       {int, 2}}
	,{'bucket.bucket2.tokens.min',   {int, 1}}
	,{'bucket.bucket2.tokens.max',   {int, 6}}
	,{'bucket.bucket2.interval',	 {int, 24*60*1000}}
	,{'bucket.bucket2.interval.min', {int, 24*60*1000}}
	
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


%--------------------------------------------
%%%%%%%%%%%% POLICER RELATED %%%%%%%%%%%%%%%%
%--------------------------------------------

policers() ->
[
 	mswitch_error
].

policer_bucket_names() ->
[
 	bucket1, bucket2
].






%% @doc Retrieves the default Value for Var
%%
%% @spec get_default(Key) -> {Key, {Type, Value}} | {}
%% where
%%		Key   = atom()
%%      Value = atom() | list()
%%		Type  = atom()
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
	{Key, {_Type, Value}}=Default,
	put({param, Key}, Value),
	put_defaults(Rest).





validate_param_limit(_Key, undefined) ->
	invalid;


%% @doc Validates a parameter against declared min/max limits.  
%%		If a parameter does not have declared limits, 'ok' is returned.
%%
%% Only integer values are validated.
%%
%% @spec validate_param_limit(Key, Value) -> ok | too_low | too_high | invalid_defaults | undefined | no_validation_possible
%%
validate_param_limit(Key, Value) ->
	TypedValue=get_default(Key),
	validate_param_limit(Key, Value, TypedValue).
	

validate_param_limit(_Key, _Value, {}) ->
	undefined;

validate_param_limit(Key, Value, {int, Value})  when is_integer(Value) ->
	try
		{int, Pmin}=get_min(Key),
		{int, Pmax}=get_max(Key),
		try
			validate_limit(Value, Pmin, Pmax)
		catch
			_:_ ->
				invalid_defaults
		end
	catch
		_:_ ->
			?LOG:log(config, error, "Invalid configuration: Key: ", [Key])
	end;

validate_param_limit(Key, _Value, {Type, _DefaultValue}) ->
	io:format("validate_param_limit: invalid, Key:~p Type:~p~n",[Key, Type]),
	no_validation_possible.
													   



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


%% @doc Retrieves the 'min' value for Key in the Defaults
%%
%% @spec get_min(Key) -> {Key, TypedValue}
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
%% @spec get_max(Key) -> {Key, TypedValue}
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
	TypedDefault=?TOOLS:kfind(Vara, defaults()),
	get_special(Pattern, Key, TypedDefault).


get_special(_Pattern, _Key, {}) ->
	undefined;
	

get_special(_Pattern, Key, TypedDefaultValue) ->
	%%io:format("get_special: Key:<~p> typed: ~p~n",[Key, TypedDefaultValue]),	
	{Key, TypedDefaultValue}.

  
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



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% Verifies that each parameter
%% - has a type
%% - the type is valid
%% - an 'int' comes with a 'min' default
%% - has a 'description'
check() ->
	Defaults=defaults(),
	check(Defaults).

check([]) ->
	finished_check;

check(Defaults) ->
	[Default|Rest] = Defaults,
	check_default(Default),
	check(Rest).

check_default({}) ->
	r("Found an empty entry");	

check_default({_Key, {string, _Value}}) ->	ok;	
check_default({_Key, {atom, _Value}}) ->	ok;
check_default({Key,  {int, Value}}) when is_integer(Value)-> 
	Presult = has_pattern(['.min', '.max'], Key),
	case Presult of
		true -> ok;
		false->

			Result=get_min(Key),
			case Result of
				undefined -> r("Expecting 'min' default for key[~p]", [Key]);
				_         -> ok
			end
	end;

check_default({Key,  {int, Value}}) -> 
	r("Expecting 'int' type for Key[~p] got[~p]", [Key, Value]);


check_default({_Key, {_Type, _Value}}) ->
	ok.

r(M) ->
	io:format(M++"~n").

r(M, P) ->
	io:format(M++"~n", P).



%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

test() ->
	check().

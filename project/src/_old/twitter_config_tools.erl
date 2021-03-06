%% Author: Jean-Lou Dupont
%% Created: 2009-08-25
%% Description: TODO: Add description to twitter_config_tools
-module(twitter_config_tools).
-compile(export_all).

-define(DEFAULTS, twitter_defaults).
-define(LOG,      twitter_log).
-define(TOOLS,    twitter_tools).


%% @doc Retrieves the default Value for Var
%%
%% @spec get_default(Key) -> {Key, {Type, Value}} | {}
%% where
%%		Key   = atom()
%%      Value = atom() | list()
%%		Type  = atom()
%%
get_default(Key) ->
	Defaults=?DEFAULTS:defaults(),
	?TOOLS:kfind(Key, Defaults).


%% @doc Puts all the default values in the process dictionary.
%%		All Keys are stored using the pattern {param, Key}.
%%
%% @spec put_defaults() -> void()
%%
put_defaults() ->
	Defaults=?DEFAULTS:defaults(),
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
	{_, TypedValue}=get_default(Key),
	%%io:format("validate_param_limit: key[~p] Value[~p] Typedp[~p]~n", [Key, Value, TypedValue]),
	validate_param_limit(Key, Value, TypedValue).
	

validate_param_limit(_Key, _Value, {}) ->
	undefined;

validate_param_limit(Key, Value, {int, _DefaultValue})  when is_integer(Value) ->
	io:format("validate_param_limit: key[~p]~n",[Key]),
	try
		MinResult=get_min(Key),
		MaxResult=get_max(Key),
		io:format("validating with limits: Value[~p] Rmin[~p] Rmax[~p]~n",[Value, MinResult,MaxResult]),
		try
			try_validate_limit(Key, Value, MinResult, MaxResult)
		catch
			X:Y ->
				io:format("validate_param_limit: X[~p] Y[~p]~n",[X,Y]),
				?LOG:log(error, "Error whilst validating key: ", [Key])
		end
	catch
		_:_ ->
			?LOG:log(error, "Invalid configuration: Key: ", [Key])
	end;
	


validate_param_limit(Key, _Value, {Type, _DefaultValue}) ->
	io:format("validate_param_limit: invalid, Key:~p Type:~p~n",[Key, Type]),
	no_validation_possible.




try_validate_limit(Key, Value, {_, {int, Min}}, {_, {int, Max}} ) ->
	Result=validate_limit(Value, Min, Max),
	process_validate_result(Key, Result);

try_validate_limit(Key, Value, {_, {int, Min}}, _) ->
	Result=validate_limit(Value, Min, undefined),
	process_validate_result(Key, Result);

try_validate_limit(Key, Value, _, {_, {int, Max}}) ->
	Result=validate_limit(Value, undefined, Max),
	process_validate_result(Key, Result);

try_validate_limit(Key, _Value, _MinResult, _MaxResult) ->
	?LOG:log(error, "Invalid 'min' and/or 'max' defaults for key: ",[Key]).



process_validate_result(_Key, ok) -> ok;
process_validate_result(Key, invalid) ->
	?LOG:log(error, "Value invalid for key: ",[Key]);

process_validate_result(Key, too_low) ->
	?LOG:log(error, "Value 'too low' for key: ", [Key]);

process_validate_result(Key, too_high) ->
	?LOG:log(error, "Value 'too high' for key: ", [Key]);

process_validate_result(Key, _Other) ->
	?LOG:log(critical, "Internal error whilst validating configuration key: ", [Key]).

			
	




%% @doc Validates a limit
%%
%% @spec validate_limit(Value, Min, Max) -> invalid | too_low | too_high | ok
%%
validate_limit(_Value, undefined, undefined) ->
	ok;

%% Just lower limit
validate_limit(Value, Min, undefined) ->
	cmp(min, Value, Min);

%% Just upper limit
validate_limit(Value, undefined, Max) ->
	cmp(max, Value, Max);

%% Both limits
validate_limit(Value, Min, Max) when is_integer(Min), is_integer(Max) ->
	R1=cmp(min, Value, Min),
	R2=cmp(max, Value, Max),
	resolve_cmp(R1, R2);

%% CATCH-ALL
validate_limit(V, Min, Max) ->
	?LOG:log(critical, "Internal error in 'validate_limit': {V, Min, Max}", [V, Min, Max]),
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



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% VALIDATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

%% Verifies that each parameter
%% - has a type
%% - the type is valid
%% - an 'int' comes with a 'min' default
%% - has a 'description'
check() ->
	Defaults=?DEFAULTS:defaults(),
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

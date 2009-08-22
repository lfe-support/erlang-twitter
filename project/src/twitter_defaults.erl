%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Default configuration parameters
%%
-module(twitter_defaults).
-compile(export_all).

-define(TOOLS, twitter_tools).


defaults() ->
	[
	 {refresh_limit_status, 5*60*1000},
	 {threshold1,           25}
	 ].

descriptions() ->
	[
	  {refresh_limit_status, 
	  	"Refresh interval (in ms) for querying Twitter about the rate_limit_status of the user"}
	 ,{threshold1,
	  	"Threshold at which point updates of lower importance than 1 (e.g. >1) will be discarded. "
		  ++"This threshold corresponds to _remaining_ amount of API calls left for the user."}

	 ].


%% Retrieves the default Value for Var
%%
%% @spec get_default(Key) -> {Key, Value} | {}
%% where
%%		Key=atom()
%%      Value=atom() | list()
%%
get_default(Key) ->
	Defaults=defaults(),
	?TOOLS:kfind(Key, Defaults).


%% Puts all the default values
%% in the process dictionary.
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


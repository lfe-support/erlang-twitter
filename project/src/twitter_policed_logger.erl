%% Author: Jean-Lou Dupont
%% Created: 2009-08-24
%% Description: Logger with contextual policing.
%%
-module(twitter_policed_logger).
-compile(export_all).

-define(LOGGER,  twitter_log).
-define(POLICER, twitter_policer).



%%
%% API Functions
%%

%% @doc Initialization
%%
%% @spec init() -> void()
%%
init() ->
	?POLICER:init().


%% @doc Log a Message with Severity and
%%      policed through Context.
%%
%% @spec log(Context, Severity, Msg) -> void()
%% where
%%	Context = atom()
%%	Severity = atom()
%%	Msg = string() | atom() | integer() | list()
%%
log(Context, Severity, Msg) ->
	ok.



%%
%% Local Functions
%%


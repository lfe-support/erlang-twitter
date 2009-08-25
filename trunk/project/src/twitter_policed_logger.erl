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
init() ->
	ok.


log(Context, Severity, Msg) ->
	ok.



%%
%% Local Functions
%%


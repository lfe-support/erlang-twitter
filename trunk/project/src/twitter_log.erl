%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Logging facility
-module(twitter_log).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 init/1
		 ]).

%%
%% API Functions
%%
init(LogName) when length(LogName)==0 ->
	init(twitter);

init(LogName) ->
	ok.



%%
%% Local Functions
%%


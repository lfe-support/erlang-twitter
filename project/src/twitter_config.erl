%% Author: Jean-Lou Dupont
%% Created: 2009-08-28
%% Description: Configuration Agent
%%
%% @doc 
%% = Configuration Agent =
%%
%% == Duties ==
%% <ul>
%%  <li>Listen for 'reload' system event</li>
%%  <li>Listen for ''</li>
%%  <li></li>
%% </ul>
%%
%%
%%
%% <ul>
%%  <li></li>
%% </ul>

-module(twitter_config).

%%
%% API functions
%%
-export([
		 ]).

%%
%% Config Functions
%%
-export([
		 defaults/0,
		 blacklist/0
		 ]).

%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  CONFIG  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

%% @doc List of parameters which cannot be customized
%%
%% @spec blacklist() -> list()
%%
blacklist() ->
	[].

%% @doc List of default parameters for the module
%%
%% @spec defaults() -> list()
%%
defaults() ->
	[].






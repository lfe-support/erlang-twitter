%% Author: Jean-Lou Dupont
%% Created: 2009-08-03
%% Description: TODO: Add description to tools
-module(tools).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
		 is_running/0,
		 extract_host/1
		 ]).

%%
%% API Functions
%%
is_running() ->
	Regs = erlang:registered(),
	lists:member(twitter, Regs).

extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	

%%
%% Local Functions
%%


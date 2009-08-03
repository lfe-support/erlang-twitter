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
		 is_running/1,
		 extract_host/0,
		 extract_host/1,
		 make_node/1,
		 make_node/2,
		 to_string/1
		 ]).

-export([
		 gen_auth_header/2,
		 gen_auth/2
		 ]).

%%
%% API Functions
%%
is_running(Node) ->
	Regs = erlang:registered(),
	lists:member(Node, Regs).



extract_host() ->
	extract_host(node()).

extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).
	


make_node(Name) ->
	make_node(Name, node()).

make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=tools:extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).
	



to_string(StringTerm) ->
	Binary=erlang:iolist_to_binary(StringTerm),
	erlang:binary_to_list(Binary).
	
	

gen_auth_header(Username, Password) ->
	"Basic "++gen_auth(Username, Password).
	
gen_auth(Username, Password) ->
	StringToEncode=Username++":"++Password,
	base64:encode_to_string(StringToEncode).
	
	


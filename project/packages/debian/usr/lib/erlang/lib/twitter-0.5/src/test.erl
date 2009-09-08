%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: TODO: Add description to test
-module(test).

-compile(export_all).

-include("xmerl.hrl").

-define(INPUT, 
		"<?xml version='1.0' encoding='UTF-8'?>"++
			"<hash>"++
				"<remaining-hits type='integer'>19933</remaining-hits>"++
				"<hourly-limit type='integer'>20000</hourly-limit>"++
				"<reset-time type='datetime'>2009-04-08T21:57:23+00:00</reset-time>"++
				"<reset-time-in-seconds type='integer'>1239227843</reset-time-in-seconds>"++
			"</hash>").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
go() ->
	{XML, Rest} = xmerl_scan:string(?INPUT, [{space, preserve}]),
	io:format("~p~n", [XML]),
	%io:format("~p~n", [Rest]),
	XML#xmlElement.namespace.
	

xml() ->
	{XML, Rest} = xmerl_scan:string(?INPUT, [{space, preserve}]),
	XML.

ns() ->
	Xml=xml(),
	Xml#xmlElement.nsinfo.

ct() ->
	Xml=xml(),
	Xml#xmlElement.content.



input() ->
	?INPUT.


%%
%% Local Functions
%%


%% Author: Jean-Lou Dupont
%% Created: 2009-09-03
%% Description: Processes responses from Twitter
%%
-module(twitter_response).
-compile(export_all).

-define(XML, twitter_xml).

%%
%% API functions
%%
-export([
		 ]).

%% ----------------------       ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------       ------------------------------

process(Response) ->
	Body=extract_response_body(Response),
	%io:format("body: ~p~n",[Body]),
	try
		{elements, Elements}=?XML:process(Body),
		extract_name_value_pairs(Elements)
	catch
		_:_ ->
			{error, cannot_process}
	end.
	

%% @doc Returns the tuple pairs of the response or {error, Reason}
%%
process('account.rate_limit_status', Response) ->
	Body=extract_response_body(Response),
	try
		{elements, Elements}=?XML:process(Body),
		extract_name_value_pairs(Elements)
	catch
		_:_ ->
			{error, cannot_process}
	end.


extract_name_value_pairs(Elements) ->
	do_extract_name_value_pairs(Elements, []).

do_extract_name_value_pairs([], Acc) -> Acc;
do_extract_name_value_pairs([Element|Elements], Acc) ->
	%io:format("do_extract: Element<~p>~n", [Element]),
	Name =?XML:get_element_name(Element),
	Value=?XML:get_element_value(Element),
	NewAcc=maybe_add_pair(Acc, Name, Value),
	do_extract_name_value_pairs(Elements, NewAcc).


maybe_add_pair(List, {error, _}, {error, _}) ->
	List;

maybe_add_pair(List, {name, Name}, {value, Value}) ->
	List++[{Name, Value}].	


extract_response_body({_Status, _Headers, Body}) ->
	try
		erlang:binary_to_list(Body)
	catch
		_:_ ->
			{error, cannot_extract_body}
	end;


extract_response_body(_) ->
	{error, cannot_find_body}.




%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  EXAMPLES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------

'account.rate_limit_status'() ->
		"<?xml version='1.0' encoding='UTF-8'?>"++
			"<hash>"++
				"<remaining-hits type='integer'>19933</remaining-hits>"++
				"<hourly-limit type='integer'>20000</hourly-limit>"++
				"<reset-time type='datetime'>2009-04-08T21:57:23+00:00</reset-time>"++
				"<reset-time-in-seconds type='integer'>1239227843</reset-time-in-seconds>"++
			"</hash>".

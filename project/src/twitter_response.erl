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
	process_body(Body).

process_body(Body) ->
	%io:format("body: ~p~n",[Body]),
	try
		{elements, Elements}=?XML:process(Body),
		extract_name_value_pairs(Elements)
	catch
		X:Y -> {error, {cannot_process, X, Y}}
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
	List++[{Name, Value}];

maybe_add_pair(List, _, _) ->
	List.


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


'statuses.update'() ->
"<?xml version='1.0' encoding='UTF-8'?>
<status>
	<created_at>Tue Apr 07 22:52:51 +0000 2009</created_at>
	<id>1472669360</id>
	<text>At least I can get your humor through tweets. RT @abdur: I don't mean this in a bad way, but genetically speaking your a cul-de-sac.</text>
	<source><a href='http://www.tweetdeck.com/'>TweetDeck</a></source>
	<truncated>false</truncated>
	<in_reply_to_status_id>1472669230</in_reply_to_status_id>
	<in_reply_to_user_id>10759032</in_reply_to_user_id>
	<favorited>false</favorited>
	<in_reply_to_screen_name></in_reply_to_screen_name>
	<!-- Not yet part of the current payload.  [COMING SOON] -->
	<geo xmlns:georss='http://www.georss.org/georss'>
		<georss:point>37.78029 -122.39697</georss:point>
	</geo>
	<user>
		<id>1401881</id>
		<name>Doug Williams</name>
		<screen_name>dougw</screen_name>
		<location>San Francisco, CA</location>
		<description>Twitter API Support. Internet, greed, users, dougw and opportunities are my passions.</description>
		<profile_image_url>http://s3.amazonaws.com/twitter_production/profile_images/59648642/avatar_normal.png</profile_image_url>
		<url>http://www.igudo.com</url>
		<protected>false</protected>
		<followers_count>1027</followers_count>
		<profile_background_color>9ae4e8</profile_background_color>
		<profile_text_color>000000</profile_text_color>
		<profile_link_color>0000ff</profile_link_color>
		<profile_sidebar_fill_color>e0ff92</profile_sidebar_fill_color>
		<profile_sidebar_border_color>87bc44</profile_sidebar_border_color>
		<friends_count>293</friends_count>
		<created_at>Sun Mar 18 06:42:26 +0000 2007</created_at>
		<favourites_count>0</favourites_count>
		<utc_offset>-18000</utc_offset>
		<time_zone>Eastern Time (US and Canada)</time_zone>
		<profile_background_image_url>http://s3.amazonaws.com/twitter_production/profile_background_images/2752608/twitter_bg_grass.jpg</profile_background_image_url>
		<profile_background_tile>false</profile_background_tile>
		<statuses_count>3390</statuses_count>
		<notifications>false</notifications>
		<following>false</following>
		<geo_enabled>true</geo_enabled> <!-- Not yet part of the current payload.  [COMING SOON] -->
		<verified>true</verified>
	</user>
</status>".

%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%  TESTS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------
t1() ->
	process_body('statuses.update'()).
	%?XML:process('statuses.update'()).


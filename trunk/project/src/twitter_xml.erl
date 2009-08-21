%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: Parses XML response
-module(twitter_xml).
-compile(export_all).

%% Includes
-include("xmerl.hrl").

%%
%% Macros
%%

%% Parses an XML string
%%
%% @spec process(Response) -> {elements, Elements} | {error, Reason} 
%% Elements = list()
%% Reason = atom()
%%
process(Response) ->
	try 
		
		{Xml, _Rest}=xmerl_scan:string(Response),
		Elements=Xml#xmlElement.content,
		{elements, Elements}
	
	catch
		_X:_Y ->
			{error, cannot_parse}
	end.

equal(X, X) ->
	true;

equal(_,_) ->
	false.

%% Retrieves a specific Attribute member
%% from a list of attributes
%%
%% @spec getattr(Element, AttrName) -> {error, Reason} | [Attributes]
%% AttrName = atom()
%% Attributes = list()
%%
%% @private
getattr(Element, AttrName) ->
	try
		Attrs=Element#xmlElement.attributes,
		Fun=fun(At) ->
					Name=At#xmlAttribute.name,
					equal(Name, AttrName)
			end,
		lists:filter(Fun, Attrs)
	catch
		_X:_Y ->
			{error, attribute_not_found}
	end.

%% Retrieves the Value of a specific Attribute
%% from the _first_ element in the list of Elements.
%% Acts as iterator by returning the "rest" of
%% the element list.
%%
%% @spec getattrvalue(Elements, AttrName) -> {error, Reason} | [Result]
%% Elements = list()
%% AttrName = atom()
%% Rest=list()
%% Value=atom() | string()
%%
%% @private
getattrvalue(Elements, AttrName) when is_list(Elements) ->
	[Element|Rest] = Elements,
	Result=getattr(Element, AttrName),
	Value=extractvalue(Result),
	package_result(Value, value, Rest);


getattrvalue(Element, AttrName) ->
	Result=getattr(Element, AttrName),
	extractvalue(Result).


%% @private
extractvalue(Attr) when is_list(Attr) ->
	[First|_Rest] = Attr,
	try
		First#xmlAttribute.value
	catch
		_X:_Y ->
			{error, value_not_found}
	end;

extractvalue(Attr) ->
	try
		Attr#xmlAttribute.value
	catch
		_X:_Y ->
			{error, attr_value_not_found}
	end.

%% Retrieves the first "text" value of
%% the _first_ Element in the list.
%% The rest of the Element list is provided
%% through {value, Value, Rest}.
%%
%% Useful as iterator.
%%
%% @spec getvalue(Elements) -> {error, Reason} | [Result]
%%
%% @private
getvalue(Elements) when is_list(Elements) ->
	[First|Rest] = Elements,
	Value=getvalue(First),
	package_result(Value, value, Rest);

getvalue(Element) ->
	try
		[TextNode]=Element#xmlElement.content,
		Value=TextNode#xmlText.value,
		{value, Value}
	catch
		_:_ ->
			{error, value_not_found}
	end.

%% Packages a Result term
%%  i.e. preserves {error, Reason} or
%%  "packages" the Result in a tuple ""wrapper""
%%
%% @private
package_result(Result, Wrapper, Rest) ->
	case Result of
		{error, Reason} ->
			{error, Reason};
		Other ->
			[{Wrapper, Other}, {rest, Rest}]
	end.

		


test1() ->
	{elements, Elements}=process('account.rate_limit_status'()),
	%%[Element|_Rest]=Elements,
	%%io:format("First: ~p~n",[Element]),
	Value=getattrvalue(Elements, type),
	io:format("Value: ~p~n",[Value]),
	Content=getvalue(Elements),
	io:format("Content: ~p~n",[Content]).


	


%%
%% Local Functions
%%

'account.rate_limit_status'() ->
		"<?xml version='1.0' encoding='UTF-8'?>"++
			"<hash>"++
				"<remaining-hits type='integer'>19933</remaining-hits>"++
				"<hourly-limit type='integer'>20000</hourly-limit>"++
				"<reset-time type='datetime'>2009-04-08T21:57:23+00:00</reset-time>"++
				"<reset-time-in-seconds type='integer'>1239227843</reset-time-in-seconds>"++
			"</hash>".

%% same response also for statuses.update
'statuses.show'() ->
	"<?xml version='1.0' encoding='UTF-8'?>
<status>
	<created_at>Tue Apr 07 22:52:51 +0000 2009</created_at>
	<id>1472669360</id>
	<text>At least I can get your humor through tweets. RT @abdur: I don't mean this in a bad way, but genetically speaking your a cul-de-sac.</text>
	<source><a href='http://www.tweetdeck.com/'>TweetDeck</a></source>
	<truncated>false</truncated>
	<in_reply_to_status_id></in_reply_to_status_id>
	<in_reply_to_user_id></in_reply_to_user_id>
	<favorited>false</favorited>
	<in_reply_to_screen_name></in_reply_to_screen_name>
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
 		<time_zone>Eastern Time (US & Canada)</time_zone>
 		<profile_background_image_url>http://s3.amazonaws.com/twitter_production/profile_background_images/2752608/twitter_bg_grass.jpg</profile_background_image_url>
 		<profile_background_tile>false</profile_background_tile>
 		<statuses_count>3390</statuses_count>
 		<notifications>false</notifications>
 		<following>false</following>
		<verified>true</verified>
	</user>
</status>".


%% one <status> element per 'update'
'statuses.user_timeline'() ->
	"<?xml version='1.0' encoding='UTF-8'?>
<statuses>
     <status>
<created_at>Tue Apr 07 22:52:51 +0000 2009</created_at>
<id>1472669360</id>
<text>At least I can get your humor through tweets. RT @abdur: I don't mean this in a bad way, but genetically speaking your a cul-de-sac.</text>
<source>&lt;a href='http://www.tweetdeck.com/'>TweetDeck&lt;/a></source>
<truncated>false</truncated>
<in_reply_to_status_id></in_reply_to_status_id>
<in_reply_to_user_id></in_reply_to_user_id>
<favorited>false</favorited>
<in_reply_to_screen_name></in_reply_to_screen_name>
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
 <time_zone>Eastern Time (US & Canada)</time_zone>
 <profile_background_image_url>http://s3.amazonaws.com/twitter_production/profile_background_images/2752608/twitter_bg_grass.jpg</profile_background_image_url>
 <profile_background_tile>false</profile_background_tile>
 <statuses_count>3390</statuses_count>
 <notifications>false</notifications>
 <following>false</following>
 <verified>true</verified>
</user>
     </status>
     ... truncated ...
</statuses>".
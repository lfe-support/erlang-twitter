%% Author: Jean-Lou Dupont
%% Created: 2009-08-22
%% Description: Default configuration parameters
%%
%% @TODO When changing defaults, a quick check should be made.
%%
%%
-module(twitter_defaults).
-compile(export_all).

-define(LOG,   twitter_log).
-define(TOOLS, twitter_tools).

%% @doc Returns the parameters blacklist
%%
%%     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%% >>  Blacklist of parameters that cannot be   <<
%% >>  changed through the configuration file   <<
%% >>  {param, Key, Value} pattern.             <<
%%     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%
%% @spec blacklist() -> list()
blacklist() ->
[
	%% Obviously, we do not want the user 
	%% to change the permissible limits...
	 refresh_mswitch_min
	,refresh_mswitch_max

	%% List of policers: provided just for debugging
	,policers

	%% Policer to Bucket Name associations
	,'policer.mswitch_error.bucket1'
	,'policer.mswitch_error.bucket2'

	%% Policer intervals
	,'bucket.bucket1.interval'
	,'bucket.bucket2.interval'
	
].



defaults() ->
[
 	{user, {nstring, ""}},
	{pass, {nstring, ""}},
	
	{refresh_limit_status,         {int, 5*60*1000}}
	,{'refresh_limit_status.min',  {int, 5*60*1000}}

	,{threshold1,          {int, 25}}
	,{'threshold1.min',    {int, 25}}

	,{refresh_mswitch,     {int, 10*1000}}
	,{refresh_mswitch.min, {int, 10*1000}}
	,{refresh_mswitch.max, {int, 60*1000}}

	%%%%%%%%% POLICERS %%%%%%%%%%%%%%%%%%%%%%%%%

	%% MSWITCH related
	,{'policer.mswitch_error.bucket1', {atom, bucket1}}
	,{'policer.mswitch_error.bucket2', {atom, bucket2}}
	
	,{'bucket.bucket1.tokens',       {int, 1}}
	,{'bucket.bucket1.tokens.min',   {int, 1}}
	,{'bucket.bucket1.tokens.max',   {int, 2}}
	,{'bucket.bucket1.interval',	 {int, 60*1000}}
	,{'bucket.bucket1.interval.min', {int, 60*1000}}

	,{'bucket.bucket2.tokens',       {int, 2}}
	,{'bucket.bucket2.tokens.min',   {int, 1}}
	,{'bucket.bucket2.tokens.max',   {int, 6}}
	,{'bucket.bucket2.interval',	 {int, 24*60*1000}}
	,{'bucket.bucket2.interval.min', {int, 24*60*1000}}
	
].


descriptions() ->
[
	{refresh_limit_status, 
	  	"Refresh interval (in ms) for querying Twitter about the rate_limit_status of the user."}
	,{threshold1,
	  	"Threshold at which point updates of lower importance than 1 (e.g. >1) will be discarded. "
		  ++"This threshold corresponds to _remaining_ amount of API calls left for the user."}
	,{refresh_mswitch,
	  	"Refresh interval (in ms) for synching with the mswitch."}
	,{refresh_mswitch_min,
	  	"Minimum refresh interval (in ms) for synching with the mswitch"}
	,{refresh_mswitch_max,
	  	"Maximum refresh interval (in ms) for synching with the mswitch"}

].


%--------------------------------------------
%%%%%%%%%%%% POLICER RELATED %%%%%%%%%%%%%%%%
%--------------------------------------------

policers() ->
[
 	mswitch_error
].

policer_bucket_names() ->
[
 	bucket1, bucket2
].


%-------------------------------------------------
%%%%%%%%%%%% LOCAL MESSAGE SWITCH %%%%%%%%%%%%%%%%
%-------------------------------------------------

msgswitch() ->
[
   %% Type,   Subscribers
	{ reload, []}
].




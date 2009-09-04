%% Author: Jean-Lou Dupont
%% Created: 2009-08-26
%% Description: Supervisor
%%
-module(twitter_sup).

-behavior(supervisor).


%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([
	 init/1,
	 start_link/0,
	 start_link/1
        ]).

%% ====================================================================
%% Server functions
%% ====================================================================
start_link() ->
	start_link([]).

start_link(Args) ->
	%ResultI=inets:start(),
	process_flag(trap_exit, true),
	ResultS=supervisor:start_link({local, ?MODULE}, ?MODULE, Args),
	%io:format("Results: i<~p> s<~p>~n", [ResultI, ResultS]),
	ResultS.

		

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(_Args) ->

	
	%% Add all modules here
	%% List the modules that require HWSWITCH bus access
	HS_Modules = [twitter_log, twitter_logpolicer, twitter_clock, twitter_config, twitter_appctl, twitter_snooper, 
				  twitter, twitter_status, twitter_updater],
	
	%% List the modules that require configuration
	%%
	CF_Modules = [twitter_log, twitter_logpolicer, twitter_appctl, twitter_snooper, twitter, twitter_status, twitter_updater ],
	
    %Child_inets = {inets,{inets, start,[]}, permanent,2000,worker,[inets]},
	
    Child_logger = {twitter_log,{twitter_log, start_link,[{logfilename, "/var/log/twitter.log"}]},
	      permanent,2000,worker,[twitter_log]},

    Child_logpolicer = {twitter_logpolicer,{twitter_logpolicer, start_link,[]},
	      permanent,2000,worker,[twitter_logpolicer]},
	
    Child_switch = {twitter_hwswitch,{twitter_hwswitch, start_link,[mods, HS_Modules]},
	      permanent,2000,worker,[twitter_hwswitch]},

    Child_clock = {twitter_clock,{twitter_clock, start_link,[]},
	      permanent,2000,worker,[twitter_clock]},

	Child_app = {twitter_appctl,{twitter_appctl, start_link,[CF_Modules]},
	      permanent,2000,worker,[twitter_appctl]},

	Child_snoop = {twitter_snooper,{twitter_snooper, start_link,[]},
	      permanent,2000,worker,[twitter_snooper]},
	
	Child_config = {twitter_config,{twitter_config, start_link,[CF_Modules]},
	      permanent,2000,worker,[twitter_config]},

	Child_twitter = {twitter,{twitter, start_link,[]},
	      permanent,2000,worker,[twitter]},

	Child_status = {twitter_status,{twitter_status, start_link,[]},
	      permanent,2000,worker,[twitter_status]},
	
	Child_updater = {twitter_updater,{twitter_updater, start_link,[]},
	      permanent,2000,worker,[twitter_updater]},
	
	Children = [Child_logger, Child_logpolicer, Child_switch, Child_clock, Child_app, Child_snoop, 
				Child_config ,Child_twitter, Child_status, Child_updater   ],
	
	
    {ok,{{one_for_one,5,1}, Children }}.


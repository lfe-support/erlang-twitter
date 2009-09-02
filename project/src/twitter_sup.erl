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
	process_flag(trap_exit, true),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(_Args) ->

	
	%% Add all modules here
	Modules = [twitter_log, twitter_hwswitch, twitter_clock, twitter_config, twitter_app, twitter_snooper ],
	
	
	
    Child_logger = {twitter_log,{twitter_log, start_link,[{logfilename, "/var/log/twitter.log"}]},
	      permanent,2000,worker,[twitter_log]},

    Child_switch = {twitter_hwswitch,{twitter_hwswitch, start_link,[mods, Modules]},
	      permanent,2000,worker,[twitter_hwswitch]},

    Child_clock = {twitter_clock,{twitter_clock, start_link,[]},
	      permanent,2000,worker,[twitter_clock]},

	Child_app = {twitter_app,{twitter_app, start_link,[Modules]},
	      permanent,2000,worker,[twitter_app]},

	Child_snoop = {twitter_snooper,{twitter_snooper, start_link,[]},
	      permanent,2000,worker,[twitter_snooper]},
	
	Child_config = {twitter_config,{twitter_config, start_link,[Modules]},
	      permanent,2000,worker,[twitter_mswitch_config]},
	
	
	Children = [Child_logger, Child_switch, Child_clock, Child_app, Child_snoop, Child_config   ],
	
	
    {ok,{{one_for_one,5,1}, Children }}.


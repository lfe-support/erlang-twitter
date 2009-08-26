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
	process_flag(trap_exit,true),
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init(_Args) ->
	
    Child_logger = {twitter_log,{twitter_log, start_link,[{logfilename, "/var/log/twitter.log"}]},
	      permanent,2000,worker,[twitter_log]},


	
    {ok,{{one_for_one,5,1}, [
							 Child_logger

							]}}.


%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: 
-module(twitter_rpc).
-compile(export_all).

%%
%% MACROS
%%
-define(SUPPORTED_CMDS, [getapiversion, reload, status, getstats, getparams]).
-define(API_VERSION, 1).
-define(MNG,        twitter_mng).
-define(SERVER,     twitter).
-define(TIMEOUT,    1000).



%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% RPC handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------

%% Retrieves the supported commands through the RPC interface
%%
handle_rpc(ReplyTo, _FromNode, getcmds) ->
	rpc_reply(ReplyTo, {cmds, ?SUPPORTED_CMDS});


%% Reloads the configuration from file
%%
handle_rpc(ReplyTo, _FromNode, reload) ->
	Result=?MNG:load_config(),
	rpc_reply(ReplyTo, Result);

%% Retrieves the statistics
%%
handle_rpc(ReplyTo, _FromNode, getstats) ->
	Stats=?MNG:get_stats(),
	rpc_reply(ReplyTo, {stats, Stats});

%% Retrieves the parameters currently in-force
%%
handle_rpc(ReplyTo, _FromNode, getparams) ->
	Params=?MNG:get_dicparams(),
	rpc_reply(ReplyTo, {params, Params});

%% Retrieves the API version
%%
handle_rpc(ReplyTo, _FromNode, getapiversion) ->
	rpc_reply(ReplyTo, {apiversion, ?API_VERSION});

%% CATCH-ALL
%%
handle_rpc(ReplyTo, _, _) ->
	rpc_reply(ReplyTo, {error, invalid_request}).


%% Validates if the RPC Command is supported
%%
rpc_validate_command(Command) when is_atom(Command) ->
	lists:member(Command, ?SUPPORTED_CMDS);

rpc_validate_command(_) ->
	invalid.


%% Replies to an RPC call.
%% The recipient loop of the message normally sits
%% in the code of the function 'rpc'.
%%
rpc_reply(ReplyTo, Message) ->
	ReplyTo ! {rpc_reply, Message}.

	
%% ----------------------                 ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCAL FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                 ------------------------------


%% @private
rpc(Q) ->
	FromNode=node(),
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%?TOOLS:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	?SERVER ! {rpc, self(), {FromNode, Q}},
	receive
		{rpc_reply, Reply} ->
			Reply;
	
		Other ->
			error_logger:error_msg("~p rpc: received [~p]~n", [?MODULE, Other]),
			{error, rpcerror}
	
	after ?TIMEOUT ->
			error_logger:error_msg("~p: rpc: timeout~n", [?MODULE]),
			{error, rpc_timeout}
	end.


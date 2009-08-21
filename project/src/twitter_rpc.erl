%% Author: Jean-Lou Dupont
%% Created: 2009-08-21
%% Description: 
-module(twitter_rpc).
-compile(export_all).

%%
%% MACROS
%%
-define(MNG, twitter_mng).
-define(SERVER, twitter).
-define(TIMEOUT, 1000).



%% ----------------------              ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% RPC handling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------              ------------------------------


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

%% CATCH-ALL
%%
handle_rpc(ReplyTo, _, _) ->
	rpc_reply(ReplyTo, {error, invalid_request}).


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


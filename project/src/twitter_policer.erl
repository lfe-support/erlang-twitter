%% Author: Jean-Lou Dupont
%% Created: 2009-07-24
%% Description: Token policer
%%
-module(twitter_policer).

-define(DEFAULTS, twitter_defaults).
-define(TOOLS,    twitter_tools).
-define(MNG,      twitter_mng).

%%
%% Exported Functions
%%
-export([
		 init/0,
		 start/0,
		 get_policers/0,
		 create_token_policer/2,
		 police/4,
		 
		 get_policer_config/1
		 ]).

%% Local functions
-export([
		 loop/0,
		 do_policing/6
		 ]).

%% TESTS
-export([
		 test/0,
		 test2/0
		 ]).


start() ->
	Pid=spawn_link(?MODULE, loop, []),
	register(?MODULE, Pid),
	{ok, Pid}.	

%% @doc start the policer process iff not already started
start_if_not() -> 
	start_if_not(erlang:whereis(?MODULE)).

start_if_not(undefined) -> 
	start();

start_if_not(Pid) -> 
	case erlang:is_process_alive(Pid) of
		false -> start();
		true  -> already_started
	end.



%% @doc Initializes all required policers
%%
%% @spec init() -> void()
%%
init() ->
	Policers=?DEFAULTS:policers(),
	init(Policers).

init([]) ->
	ok;

init(Policers) when is_list(Policers) ->
	[Policer|Rest]=Policers,
	init(Policer),
	init(Rest);

init(Policer) ->
	Config=get_policer_config(Policer),
	%%io:format("init: policer: ~p config:~p ~n",[Policer, Config]),	
	create_token_policer(Policer, Config).



%% @doc Retrieves the active policers
%%
%% @spec get_policers() -> list()
%%
get_policers() ->
	call(get_policers).





%% @doc Creates a token based policer
%%      with a specified number of cascading buckets.
%%
%% @spec create_token_policer(Policer, Buckets) -> void()
%% where
%%	Policer = atom() %% unique policer id
%%  Buckets= Bucket() | [Bucket()]
%%  @type Bucket = {MaxTokens, PeriodDuration}
%%  MaxTokens = integer()
%%  PeriodDuration = integer() % in milliseconds
%%  
create_token_policer(Policer, Buckets) ->
	start_if_not(),
	?MODULE ! {create, Policer, Buckets}.


%% @doc Apply policing
%%		If no policer is found, 'pass' is the default.
%%
%% @spec police(Policer, ReplyTo, PassMsg, DropMsg) -> void()
%% where
%%	Policer = atom()  %%unique policer identifier
%%	ReplyTo = pid()
%%	PassMsg = tuple()
%%  DropMsg = tuple()
%%
police(Policer, ReplyTo, PassMsg, DropMsg) ->
	?MODULE ! {police, Policer, ReplyTo, PassMsg, DropMsg}.


%%
%% Local Functions
%%

loop() ->
	receive
		{command, From, Command} ->
			handle_command(From, Command);
		
		{create, Policer, Buckets} ->
			io:format("create: policer[~p] Buckets[~p]~n",[Policer, Buckets]),
			add_buckets(Policer, Buckets);
		
		{police, Policer, ReplyTo, PassMsg, DropMsg} ->
			Buckets=?TOOLS:getvar({buckets, Policer}, []),
			do_policing(pass, Policer, Buckets, ReplyTo, PassMsg, DropMsg);

		stop ->
			exit(ok)
	end,
	loop().



%% @doc Adds a new policer specification
%%		whilst filtering duplicates.
%%
add_buckets(Policer, Buckets) ->
	%% Useful for outrospection
	?TOOLS:add_to_var_list({param, policers}, Policer),
	
	%% the policer configuration per-se
	?TOOLS:add_to_var_list({buckets, Policer}, Buckets).
  

policing_reply(To, Msg) ->
	try
		To ! Msg
	catch
		_:_ ->
			?MNG:inc_stat(error_policing_reply)
	end.



%% End of list *OR* no policer configured
%% Result: 'pass' by default
%%
do_policing(pass, _Policer, [], ReplyTo, PassMsg, _) ->
	case is_pid(ReplyTo) of
		%%true ->	ReplyTo ! PassMsg;
		true -> policing_reply(ReplyTo, PassMsg);
		_    -> pass
	end;


%% Stop recursion on first 'drop' decision
do_policing(drop, _Policer, _, ReplyTo, _, DropMsg) ->
	case is_pid(ReplyTo) of
		%%true -> ReplyTo ! DropMsg;
		true -> policing_reply(ReplyTo, DropMsg);
		_    -> drop
	end;


do_policing(pass, Policer, Bucket, _, _, _) when is_tuple(Bucket) ->
	
	{MaxTokens, Period} = Bucket,
	CurrentTime=now(),
	
	%% retrieve start of interval
	StartPeriod=base:getvar({start_period, Policer}, CurrentTime),
	
	%% current token count
	Tokens=base:getvar({tokens, Policer}, 0),
	
	%% is interval finished?
	Diff=timer:now_diff(CurrentTime, StartPeriod),
	
	if 
		Diff > Period ->
			%% end of period... start a new one
			put({start_period, Policer}, CurrentTime),
			Tokens2=0,
			ok;
		
		true ->
			%% in the period, add a token to the bucket
			%%  and see if overflow occurs
			Tokens2=Tokens
		 
	end,
	
	NewCount = Tokens2 + 1,
	put({tokens, Policer}, NewCount),
	
	if 
		NewCount > MaxTokens ->
			%% Drop
			%%io:format("drop: id: ~p, tokens: ~p~n", [Policer, NewCount]),
			?MNG:inc_stat({policer, Policer, drop}),
			drop;
		
		true ->
			%%io:format("pass: id: ~p, tokens: ~p~n", [Policer, NewCount]),
			?MNG:inc_stat({policer, Policer, pass}),
			pass
	end;




do_policing(PreviousResult, Policer, Buckets, ReplyTo, PassMsg, DropMsg) when is_list(Buckets) ->
	[Bucket|Rest] = Buckets,
	Result = do_policing(PreviousResult, Policer, Bucket, ReplyTo, PassMsg, DropMsg),
	do_policing(Result, Policer, Rest, ReplyTo, PassMsg, DropMsg).



%% ----------------------            ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% MANAGEMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------            ------------------------------





%% @doc Retrieves from the process dictionary the
%%		configuration information specific to a policer.
%%
%% The configuration information consists of:
%%   BucketNames = [{policer.PolicerName.bucket1} | {policer.PolicerName.bucket2}]
%%   
%% A bucket configuration can be accessed through:
%%   Tokens   = {bucket.BucketId.tokens}
%%   Interval = {bucket.BucketId.interval}
%%
%% @spec get_policer_config(PolicerName) -> [Bucket1, Bucket2]
%%
get_policer_config(PolicerName) ->
	BucketName1=get_policer_bucket(PolicerName, 1),
	BucketName2=get_policer_bucket(PolicerName, 2),
	Bucket1=get_bucket(BucketName1),	
	Bucket2=get_bucket(BucketName2),
	policer_config_result(Bucket1, Bucket2).

policer_config_result({},{}) ->
	undefined;

policer_config_result(Bucket1,Bucket2) ->
	[Bucket1, Bucket2].


get_policer_bucket(PolicerName, BucketId) ->
	BaseName=[policer,'.',PolicerName,'.',bucket,BucketId],
	AtomName=?TOOLS:make_atom_from_list(BaseName),
	?MNG:get_param(AtomName, undefined).
	


get_bucket(undefined) -> {};

%% @doc Retrieves the Bucket associated with the identifier BucketId
%%
%% @spec get_policer_bucket(PolicerName, BucketId) -> Bucket()
%% where
%%	@type Bucket = {MaxTokens, Interval}
%%	MaxTokens= integer()
%%	Interval = integer()
get_bucket(BucketId) ->
	BaseName=[bucket,'.',BucketId,'.'],
	Token=BaseName++[tokens],
	Inter=BaseName++[interval],
	TokenAtom=?TOOLS:make_atom_from_list(Token),
	InterAtom=?TOOLS:make_atom_from_list(Inter),
	TokenMin=?DEFAULTS:get_min(TokenAtom),
	InterMin=?DEFAULTS:get_min(InterAtom),
	TokenValue=?MNG:get_param(TokenAtom, TokenMin),
	InterValue=?MNG:get_param(InterAtom, InterMin),
	{TokenValue, InterValue}.


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

call(Command) ->
	?MODULE ! {command, self(), Command},
	receive
		{reply, Response} -> Response;
		_                 -> {error, unexpected_reply}
	after 1000 ->
		{error, timeout}
	end.


%% ----------------------                  ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% COMMAND handlers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------                  ------------------------------

handle_command(From, get_policers) ->
	Policers=get({param, policers}),
	callreply(From, Policers).

callreply(From, Msg) ->
	From ! {reply, Msg}.





test() ->
	?DEFAULTS:put_defaults(),
	get_policer_config(mswitch_error).

test2() ->
	?DEFAULTS:put_defaults(),
	get_policer_config(unknown).
	
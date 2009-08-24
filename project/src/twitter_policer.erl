%% Author: Jean-Lou Dupont
%% Created: 2009-07-24
%% Description: TODO: Add description to policer
-module(twitter_policer).


%%
%% Exported Functions
%%
-export([
		 create_token_policer/1,
		 police/4
		 ]).

%% Local functions
-export([
		 loop/1,
		 do_policing/5
		 ]).

%% TESTS
-export([
		 create_token_policer/0,
		 test_police/1
		 ]).



%% @doc Creates a token based policer
%%      with a specified number of cascading buckets.
%%
%% @spec create_token_policer(Buckets) -> {ok, Pid}
%% where
%%  Buckets= Bucket() | [Bucket()]
%%  @type Bucket = {Id, MaxTokens, PeriodDuration}
%%  Id = atom() | string()     % unique identifier (used process dictionary)
%%  MaxTokens = integer()
%%  PeriodDuration = integer() % in milliseconds
%%  
create_token_policer(Buckets) ->
	Pid=spawn(?MODULE, loop, [Buckets]),
	Pid ! start,
	{ok, Pid}.


%% 
police(Policer, ReplyTo, PassMsg, DropMsg) ->
	Policer ! {police, ReplyTo, PassMsg, DropMsg}.


%%
%% Local Functions
%%

loop(Buckets) ->
	receive
		{police, ReplyTo, PassMsg, DropMsg} ->
			do_policing(pass, Buckets, ReplyTo, PassMsg, DropMsg);

		stop ->
			exit(ok)
	end,
	loop(Buckets).



%% End of list
do_policing(pass, [], ReplyTo, PassMsg, _) ->
	case is_pid(ReplyTo) of
		true ->
			ReplyTo ! PassMsg;
		_ ->
			io:format("passed~n"),
			pass
	end;


%% Stop recursion on first 'drop' decision
do_policing(drop, _, ReplyTo, _, DropMsg) ->
	case is_pid(ReplyTo) of
		true ->
			ReplyTo ! DropMsg;
		_ ->
			io:format("dropped~n"),
			drop
	end;

do_policing(pass, Bucket, _, _, _) when is_tuple(Bucket) ->
	
	{Id, MaxTokens, Period} = Bucket,
	CurrentTime=now(),
	
	%% retrieve start of interval
	StartPeriod=base:getvar({start_period, Id}, CurrentTime),
	
	%% current token count
	Tokens=base:getvar({tokens, Id}, 0),
	
	%% is interval finished?
	Diff=timer:now_diff(CurrentTime, StartPeriod),
	
	if 
		Diff > Period ->
			%% end of period... start a new one
			put({start_period, Id}, CurrentTime),
			Tokens2=0,
			ok;
		
		true ->
			%% in the period, add a token to the bucket
			%%  and see if overflow occurs
			Tokens2=Tokens
		 
	end,
	
	NewCount = Tokens2 + 1,
	put({tokens, Id}, NewCount),
	
	if 
		NewCount > MaxTokens ->
			%% Drop
			io:format("drop: id: ~p, tokens: ~p~n", [Id, NewCount]),
			drop;
		
		true ->
			io:format("pass: id: ~p, tokens: ~p~n", [Id, NewCount]),
			pass
	end;




do_policing(PreviousResult, Buckets, ReplyTo, PassMsg, DropMsg) when is_list(Buckets) ->
	[Head|Tail] = Buckets,
	Result = do_policing(PreviousResult, Head, ReplyTo, PassMsg, DropMsg),
	do_policing(Result, Tail, ReplyTo, PassMsg, DropMsg).




%% =========================================
%% Test API
%% =========================================
create_token_policer() ->
	%% Dual Token
	%% 2 in the 10 seconds interval
	%%   up to 5 in 60 seconds interval
	Buckets=[{bucket1, 2, 10*1000*1000}, {bucket2, 5, 60*1000*1000}],
	create_token_policer(Buckets).

test_police(Policer) ->
	police(Policer, undefined, passed, dropped).



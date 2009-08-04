%%
%% Twitter.hrl
%%
%% @author: Jean-Lou Dupont
%%

-export([
		 ping/0,
		 pong/0
		 ]).

-export([
		 request/1,  %% only  Method
		 request/2,  %%       Method, Params
		 request/3,  %% Auth, Method, Params
		 request/4   %% Auth, Method, Params, [optional params]
		 ]).


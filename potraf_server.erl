
-module(potraf_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("definitions.hrl").

%% 
%% API (gen_server functions)
%% 

start_link(Time_interval, Update_now) ->
    gen_server:start_link(?MODULE, [Time_interval, Update_now], []).

init([Time_interval, Update_now]) ->
    timer:send_interval(Time_interval, #potraf_req{type = update}),
    case Update_now of
	update -> 
	    gen_server:cast(self(), #potraf_req{type = update}),
	    {ok, #potraf_state{readiness = updating}};
	_ -> 
	    {ok, #potraf_state{readiness = ready}}
    end.


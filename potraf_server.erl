
-module(potraf_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("definitions.hrl").

%% 
%% API (gen_server functions)
%% 

start_link(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

init(_Args) ->
    cast(self(), #potraf_req{type = update}),
    send_interval(Time, #potraf_req{type = update}), % Time should be taken from args or from config, if there is not in args
    {ok, #potraf_state{readiness = updating}}.


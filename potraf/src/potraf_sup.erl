
%% Supervisor for PostOfficeTraffic
%% it should run and supervise informer and updater at least

-module(potraf_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

-include("definitions.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(SupName) ->
    supervisor:start_link(SupName, ?MODULE, []).

init([]) ->
    {ok, 
     {{one_for_one, 5, 60}, 		% TODO: use application config
      [{?INFORMER,
	{?INFORMER, start_link, [5000, dont_update]}, % TODO: use app config too
	permanent,
	infinity,
	worker,
	[?INFORMER]},
       {?UPDATER,
	{?UPDATER, start_link, [dont_update]},
	permanent,
	infinity,
	worker,
	[?UPDATER]}]}}.

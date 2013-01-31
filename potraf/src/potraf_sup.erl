
%% Supervisor for PostOfficeTraffic
%% it should run and supervise informer and updater at least

-module(potraf_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

-include_lib("definitions.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(SupName) ->
    supervisor:start_link(SupName, ?MODULE, []).

init([]) ->
    Shutdown_time = 1000,
    Informer = {?INFORMER,
		{?INFORMER, start_link, [application:get_env(upd_time_interval), dont_update]},
		permanent,
		Shutdown_time,
		worker,
		[?INFORMER]},
    Updater = {?UPDATER,
	       {?UPDATER, start_link, [dont_update]},
	       permanent,
	       Shutdown_time,
	       worker,
	       [?UPDATER]},
    Getters_sup = {potraf_getters_sup,
		   {potraf_getters_sup, start_link, []},
		   permanent,
		   Shutdown_time,
		   supervisor,
		   [potraf_getters_sup]},
    Adders_sup = {potraf_adders_sup,
		  {potraf_adders_sup, start_link, []},
		  permanent,
		  Shutdown_time,
		  supervisor,
		  [potraf_adders_sup]},
    Update_event_manager = {update_event_manager,
			    {update_event_manager, start_link, []},
			    permanent,
			    Shutdown_time,
			    worker,
			    dynamic},
    {ok, 
     {{one_for_one, 5, 60}, 		
      [Informer, Updater, Getters_sup, Adders_sup, Update_event_manager]
     }}.


-module(potraf_getters_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 5, 60},
	  [{potraf_getter,
	    {potraf_getter, start_link, []},
	    temporary, 
	    1000, 
	    worker, 
	    [potraf_getter]}
	  ]}}.

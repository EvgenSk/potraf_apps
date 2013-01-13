
-module(potraf_adders_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 5, 60},
	  [{potraf_adder,
	    {potraf_adder, start_link, []},
	    transient, 
	    1000, 
	    worker, 
	    [potraf_adder]}
	  ]}}.

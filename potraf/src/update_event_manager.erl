
-module(update_event_manager).

-export([start_link/0]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

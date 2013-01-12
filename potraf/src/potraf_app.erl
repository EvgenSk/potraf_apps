
%% POst Office TRAFfic application

-module(potraf_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(normal, _Args) ->
    potraf_sup:start_link().

stop(_State) ->
    ok.

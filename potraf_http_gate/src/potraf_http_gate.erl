
-module(potraf_http_gate).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


%% API

start(_Type, _Opts) ->
    {ok, Repl_type} = application:get_env(repl_type),
    {ok, NbAcceptors} = application:get_env(nbAcceptors),
    {ok, Port} = application:get_env(port),
    Dispatch = [
		{'_', [
		      {[], potraf_http_handler, [Repl_type]}
		      ]
		}],
    {ok, _} = cowboy:start_http(http, NbAcceptors, 
				[{port, Port}],
				[{dispatch, Dispatch}]),
    potraf_http_gate_sup:start_link().

stop(_State) ->
    ok.

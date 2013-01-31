
%% External interface

-module(potraf).

-export([request/2]).

-include_lib("definitions.hrl").

-define(name(ZIP), list_to_atom(lists:concat(["adder", utils:to_list(ZIP)]))).

%% API

request(get, ZIP)->
    get_data(ZIP);

request(add, {ZIP, KeyVals}) ->
    add_data(ZIP, keyvals_to_traffic(KeyVals));

request(_, _Req) ->
    undefined.

%% Internal functions

get_data(ZIP) ->
    {ok, C} = supervisor:start_child(potraf_getters_sup, []),
    {Traf_info, Timestamps} = gen_server:call(C, #potraf_req{request = get, param = ZIP}),
    zip_traf_info(Traf_info, Timestamps).

add_data(ZIP, Traffic) ->
    Adder_name = ?name(ZIP),
    supervisor:start_child(potraf_adders_sup, [{local, Adder_name}]),
    gen_server:cast(Adder_name, #potraf_req{request = add, param = {ZIP, Traffic, now()}}),
    ok.

zip_traf_info(Traf_info, Timestamps) ->
    lists:zip3(record_info(fields, traffic), 
	       tl(tuple_to_list(Traf_info)),
	       tl(tuple_to_list(Timestamps))).

keyvals_to_traffic(KeyVals) ->
    #traffic{
	      people_count = proplists:get_value(people_count, KeyVals),
	      service_time = proplists:get_value(service_time, KeyVals),
	      post_windows_count = proplists:get_value(post_windows_count, KeyVals),
	      package_windows_count = proplists:get_value(package_windows_count, KeyVals)
	    }.

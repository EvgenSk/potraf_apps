
%% External interface

-module(potraf).

-export([request/1]).

-include_lib("definitions.hrl").

-define(name(ZIP), list_to_atom(lists:concat(["adder", utils:to_list(ZIP)]))).

request(Req = #potraf_req{request = get}) ->
    get_data(Req#potraf_req.param);

request(Req = #potraf_req{request = add}) ->
    {ZIP, Traffic} = Req#potraf_req.param,
    add_data(ZIP, Traffic);

request(_Req) ->
    undefined.


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

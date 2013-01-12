
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
    {ok, C} = potraf_getter:start_link(),
    gen_server:call(C, #potraf_req{request = get, param = ZIP}).

add_data(ZIP, Traffic) ->
    Adder_name = ?name(ZIP),
    case whereis(Adder_name) of 
	undefined -> 
	    potraf_adder:start_link({local, Adder_name})
    end,
    gen_server:cast(Adder_name, #potraf_req{request = add, param = {ZIP, Traffic}}),
    ok.

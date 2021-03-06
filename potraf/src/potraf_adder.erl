
-module(potraf_adder).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("definitions.hrl").

-import(lists, [foreach/2, filter/2]).
-import(potraf_lib, [run_with_connection/2, 
		     set_expiration_time/4, 
		     trim/3, 
		     mark_for_upd/3,
		     upd_time_interval/0]).

-define(TIMEOUT, 600000).
%% 
%% API
%% 

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [], []).

init(_Args) ->
    case application:get_env(adder_life_time) of
	{ok, Life_time} -> {ok, Life_time, Life_time};
	_ -> {ok, ?TIMEOUT, ?TIMEOUT}
    end.

%% handle_call

handle_call(_Req, _Msg, Life_time) ->
    {reply, ok, Life_time, Life_time}.

%% handle_cast

handle_cast(#potraf_req{request = add, param = {ZIP, Traf_info, Timestamp}}, Life_time) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info, Timestamp),
    {noreply, Life_time, Life_time};

handle_cast(_Msg, Life_time) ->
    {noreply, Life_time, Life_time}.

%% handle_info

handle_info(timeout, Life_time)->
    {stop, normal, Life_time};

handle_info(_Message, Life_time) ->
    {noreply, Life_time, Life_time}.

%% code_change

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% terminate

terminate(normal, _State) ->
    ok.

%% 
%% Internal functions
%% 

add_traffic_info(ZIP, Traf_info) ->
    run_with_connection(fun(Connection)-> 
				add_traffic_info(Connection, ZIP, Traf_info) end, 
			?RAW_DATA).

add_traffic_info(Connection, ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf_lib:add(Connection, ZIP, Id, Val) end,
	    Traf_info).

add_timestamps_info(ZIP, Fields, Timestamp) ->
    run_with_connection(fun(Connection)-> 
				add_timestamps_info(Connection, ZIP, Fields, Timestamp) end, 
			?RAW_DATA).

add_timestamps_info(Connection, ZIP, Fields, Timestamp) ->
    foreach(fun(Field) -> potraf_lib:add_params_timestamp(Connection, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Fields, Timestamp) ->
    run_with_connection(fun(Connection)-> 
				set_last_timestamps(Connection, ZIP, Fields, Timestamp) end, 
			?RAW_DATA).


set_last_timestamps(Connection, ZIP, Fields, Timestamp) -> 
    foreach(fun(Field) -> potraf_lib:set_last_timestamp(Connection, ZIP, Field, Timestamp) end,
	    Fields).

mark_for_upd(ZIP) ->
    Updating_key = 
	case informer:get_updating_status() of
	    #data_status{status = updating} -> tmp;
	    _ -> main
	end,
    run_with_connection(fun(Connection)-> 
				mark_for_upd(Connection, Updating_key, ZIP) end,
			?RESULT).

add_traffic_and_timestamps_info(ZIP, Traf_info, Timestamp) ->
    Useful_elems = get_useful_elems(Traf_info),
    if length(Useful_elems) > 0 ->
	    mark_for_upd(ZIP),
	    add_traffic_info(ZIP, Useful_elems),
	    Useful_fields = lists:map(fun({Id, _Val})-> Id end, Useful_elems),
	    add_timestamps_info(ZIP, Useful_fields, Timestamp),
	    set_last_timestamps(ZIP, Useful_fields, Timestamp),
	    trim_data(ZIP, Useful_fields),
	    set_expiration(ZIP, Useful_fields)
    end.

get_useful_elems(Traf_info) ->
    filter(fun({_, Val})-> potraf_lib:is_useful(Val) end, 
	   ?record_to_tuplelist(traffic, Traf_info)).

trim_data(ZIP, Fields) ->
    run_with_connection(fun(Connection)-> 
				foreach(fun(Field)-> 
						trim(Connection, ZIP, Field) end,
					Fields) end,
			?RAW_DATA).
set_expiration(ZIP, Fields) ->
    run_with_connection(fun(Connection)-> 
				Expiration_time = 2 * upd_time_interval(),
				foreach(fun(Field)-> 
						set_expiration_time(Connection, ZIP, Field, Expiration_time) end,
					Fields)end,
			?RAW_DATA).

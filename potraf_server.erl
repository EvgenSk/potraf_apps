
-module(potraf_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("definitions.hrl").

%% 
%% API (gen_server functions)
%% 

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [], []).

init(_Args) ->
    {ok, ready}.

%% handle_call

handle_call(#potraf_req{type = get, param = ZIP}, _From, State) ->
    wait_data_ready(ZIP),
    Traffic = get_traffic_info(ZIP),
    Timestamps = get_timestamps(ZIP),
    {reply, {Traffic, Timestamps}, State}; % TODO: may be we need record for reply {Traffic, Timestamps}

handle_call(#potraf_req{type = add, param = {ZIP, Traf_info}}, _From, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info),
    {noreply, State}.

%% handle_cast

handle_cast(#potraf_req{type = add, param = {ZIP, Traf_info}}, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info

handle_info(_Message, State) ->
    {noreply, State}.


%% 
%% Internal functions
%% 

get_traffic_info(ZIP) ->
    get_traffic_info(potraf:get_connection(?RESULT), ZIP).

get_traffic_info(Connection, ZIP) ->
    #traffic{people_count = potraf:get(Connection, ZIP, people_count),
	     service_time = potraf:get(Connection, ZIP, service_time),
	     increment = potraf:get(Connection, ZIP, increment),
	     post_windows_count = potraf:get(Connection, ZIP, post_windows_count),
	     package_windows_count = potraf:get(Connection, ZIP, package_windows_count)}.

get_timestamps(ZIP) ->
    get_timestamps(potraf:get_connection(?RAW_DATA), ZIP).

get_timestamps(Connection, ZIP) ->
    #timestamps{people_count = potraf:get_params_timestamp(Connection, ZIP, people_count),
		service_time = potraf:get_params_timestamp(Connection, ZIP, service_time),
		increment = potraf:get_params_timestamp(Connection, ZIP, increment),
		post_windows_count = potraf:get_params_timestamp(Connection, ZIP, post_windows_count),
		package_windows_count = potraf:get_params_timestamp(Connection, ZIP, package_windows_count)}.

add_traffic_info(ZIP, Traf_info) ->
    add_traffic_info(potraf:get_connection(?RAW_DATA), ZIP, Traf_info).

add_traffic_info(Connection, ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf:add(Connection, ZIP, Id, Val) end,
	    record_to_tuplelist(traffic, Traf_info)).

add_timestamps_info(ZIP, Timestamp) ->
    add_timestamps_info(potraf:get_connection(?RAW_DATA), ZIP, Timestamp).

add_timestamps_info(Connection, ZIP, Timestamp) ->
    Fields = record_info(fields, traffic),
    foreach(fun(Field) -> potraf:add_params_timestamp(Connection, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Traf_info, Timestamp) ->
    set_last_timestamps(potraf:get_connection(?RESULT), ZIP, Traf_info, Timestamp).

set_last_timestamps(Connection, ZIP, Traf_info, Timestamp) ->
    foreach(fun({Id, Val}) -> potraf:set_last_timestamps(Connection, ZIP, Id, Timestamp) end,
	    filter(fun({_, Val})-> potraf:is_useful(Val) end, 
		   record_to_tuplelist(traffic, Traf_info))).

mark_for_upd(ZIP) ->
    Updating_key = 
	case get_updating_status() of
	    ready -> main;
	    _ -> tmp
	end,
    potraf:mark_for_upd(?RESULT, Updating_key, ZIP).

add_traffic_and_timestamps_info(ZIP, Traf_info) ->
    Timestamp = now(),
    add_traffic_info(ZIP, Traf_info),
    add_timestamps_info(ZIP, Timestamp),
    set_last_timestamps(ZIP, Traf_info, Timestamp).

get_data_status(ZIP) ->
    case get_updating_status() of
	#data_status{status = up_to_date} -> ready;
	_ -> check_need_update(ZIP)
    end.

get_updating_status() ->
    {reply, Reply} = gen_server:call(?INFORMER, #data_req{request = info, info_type = updating_status}),
    Reply.

check_need_update(ZIP) ->
    potraf:check_need_upd(potraf:get_connection(?RESULT), ZIP).
	    
wait_data_ready(ZIP) ->
    case get_data_status(ZIP) of
	ready -> ready;
	_ -> wait_data_ready(ZIP)
    end.

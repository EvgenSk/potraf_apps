
-module(potraf_client).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("definitions.hrl").

-import(lists, [foreach/2, filter/2]).

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

handle_call(#potraf_req{request = get, param = ZIP}, _From, State) ->
    wait_data_ready(ZIP),			% TODO: do it another way
    Traffic = get_traffic_info(ZIP),
    Timestamps = get_timestamps(ZIP),
    {reply, {Traffic, Timestamps}, State}; % TODO: may be we need record for reply {Traffic, Timestamps}

handle_call(#potraf_req{request = add, param = {ZIP, Traf_info}}, _From, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info),
    {reply, ok, State}.

%% handle_cast

handle_cast(#potraf_req{request = add, param = {ZIP, Traf_info}}, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info

handle_info(_Message, State) ->
    {noreply, State}.

%% code_change

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% terminate

terminate(normal, _State) ->
    ok.

%% 
%% Internal functions
%% 

get_connection(result) ->
    potraf_lib:get_connection(0);

get_connection(raw_data) ->
    potraf_lib:get_connection(1).

get_traffic_info(ZIP) ->
    get_traffic_info(get_connection(?RESULT), ZIP).

get_traffic_info(Connection, ZIP) ->
    #traffic{people_count = potraf_lib:to_list(potraf_lib:get(Connection, ZIP, people_count)),
	     service_time = potraf_lib:to_list(potraf_lib:get(Connection, ZIP, service_time)),
	     increment = potraf_lib:to_list(potraf_lib:get(Connection, ZIP, increment)),
	     post_windows_count = potraf_lib:to_list(potraf_lib:get(Connection, ZIP, post_windows_count)),
	     package_windows_count = potraf_lib:to_list(potraf_lib:get(Connection, ZIP, package_windows_count))}.

get_timestamps(ZIP) ->
    get_timestamps(get_connection(?RAW_DATA), ZIP).

get_timestamps(Connection, ZIP) ->
    #timestamps{people_count = potraf_lib:to_list(potraf_lib:get_params_timestamp(Connection, ZIP, people_count)),
		service_time = potraf_lib:to_list(potraf_lib:get_params_timestamp(Connection, ZIP, service_time)),
		increment = potraf_lib:to_list(potraf_lib:get_params_timestamp(Connection, ZIP, increment)),
		post_windows_count = potraf_lib:to_list(potraf_lib:get_params_timestamp(Connection, ZIP, post_windows_count)),
		package_windows_count = potraf_lib:to_list(potraf_lib:get_params_timestamp(Connection, ZIP, package_windows_count))}.

add_traffic_info(ZIP, Traf_info) ->
    add_traffic_info(get_connection(?RAW_DATA), ZIP, Traf_info).

add_traffic_info(Connection, ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf_lib:add(Connection, ZIP, Id, Val) end,
	    ?record_to_tuplelist(traffic, Traf_info)).

add_timestamps_info(ZIP, Timestamp) ->
    add_timestamps_info(get_connection(?RAW_DATA), ZIP, Timestamp).

add_timestamps_info(Connection, ZIP, Timestamp) ->
    Fields = record_info(fields, traffic),
    foreach(fun(Field) -> potraf_lib:add_params_timestamp(Connection, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Traf_info, Timestamp) ->
    set_last_timestamps(get_connection(?RESULT), ZIP, Traf_info, Timestamp).

set_last_timestamps(Connection, ZIP, Traf_info, Timestamp) ->
    foreach(fun({Id, _Val}) -> potraf_lib:set_last_timestamp(Connection, ZIP, Id, Timestamp) end,
	    filter(fun({_, Val})-> potraf_lib:is_useful(Val) end, 
		   ?record_to_tuplelist(traffic, Traf_info))).

mark_for_upd(ZIP) ->
    Updating_key = 
	case get_updating_status() of
	    ready -> main;
	    _ -> tmp
	end,
    potraf_lib:mark_for_upd(get_connection(?RESULT), Updating_key, ZIP).

add_traffic_and_timestamps_info(ZIP, Traf_info) ->
    Timestamp = now(),				% TODO: need to get timestamp correctly
    mark_for_upd(ZIP),
    add_traffic_info(ZIP, Traf_info),
    add_timestamps_info(ZIP, Timestamp),
    set_last_timestamps(ZIP, Traf_info, Timestamp).

get_data_status(ZIP) ->
    case get_updating_status() of
	#data_status{status = up_to_date} -> ready;
	_ -> check_need_update(ZIP)
    end.

get_updating_status() ->
    gen_server:call(?INFORMER, #data_req{request = info, info_type = updating_status}).

check_need_update(ZIP) ->
    potraf_lib:check_need_upd(get_connection(?RESULT), ZIP).
	    
wait_data_ready(ZIP) ->
    case get_data_status(ZIP) of
	ready -> ready;
	_ -> wait_data_ready(ZIP)
    end.


-module(potraf_server).
-behaviour(gen_server).

-export([start_link/2, start_link/3]).
-export([init/1, handle_call/3, handle_cast/2]).

-include("definitions.hrl").

%% 
%% API (gen_server functions)
%% 

start_link(Time_interval, Update_now) ->
    gen_server:start_link(?MODULE, [Time_interval, Update_now], []).

start_link(ServerName, Time_interval, Update_now) ->
    gen_server:start_link(ServerName, ?MODULE, [Time_interval, Update_now], []).

init([Time_interval, Update_now]) ->
    timer:send_interval(Time_interval, #potraf_req{type = update}), % may be we must use apply_interval instead of send_interval
    case Update_now of
	update -> 
	    gen_server:cast(?UPDATER, #potraf_req{type = update}),
	    {ok, #potraf_state{readiness = updating}};
	_ -> 
	    {ok, #potraf_state{readiness = ready}}
    end.

%% handle_call

handle_call(#potraf_req{type = update}, _From, State) ->
    case State of
	#potraf_state{readiness = ready} ->
	    gen_server:cast(?UPDATER, #potraf_req{type = update}),
	    {noreply, #potraf_state{readiness = updating}};
	 _ -> {noreply, State}
    end;

handle_call(#potraf_req{type = get, param = ZIP}, _From, State) ->
    wait_data_ready(ZIP),
    Traffic = get_traffic_info(ZIP),
    Timestamps = get_timestamps(ZIP),
    {reply, {Traffic, Timestamps}, State}; % TODO: may be we need record for reply {Traffic, Timestamps}

handle_call(#potraf_req{type = add, param = {ZIP, Traf_info}}, _From, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info),
    {noreply, State}.

%% handle_cast

handle_cast(#potraf_req{type = update}, State) 
  when State = #potraf_state{readiness = ready}->
    gen_server:cast(?UPDATER, #potraf_req{type = update}), % TODO: may be better would be to rise new process for updating every time
    {noreply, #potraf_state{readiness = updating}};

handle_cast(#potraf_req{type = update}, State) 
  when State = #potraf_state{readiness = updating} ->
    {noreply, State};

handle_cast(#potraf_req{type = add, param = {ZIP, Traf_info}}, State) ->
    add_traffic_info(ZIP, Traf_info),
    {noreply, State};

handle_cast(updated, _State) ->			% TODO: move 'updated' to some record?
    {noreply, #potraf_state{readiness = ready}};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info

handle_info(_Message, State) ->
    {noreply, State}.


%% 
%% Internal functions
%% 

get_traffic_info(ZIP) ->
    #traffic{people_count = potraf:get(?RESULT, ZIP, people_count),
	     service_time = potraf:get(?RESULT, ZIP, service_time),
	     increment = potraf:get(?RESULT, ZIP, increment),
	     post_windows_count = potraf:get(?RESULT, ZIP, post_windows_count),
	     package_windows_count = potraf:get(?RESULT, ZIP, package_windows_count)}.

get_timestamps(ZIP) ->
    #timestamps{people_count = potraf:get_params_timestamp(?RESULT, ZIP, people_count),
		service_time = potraf:get_params_timestamp(?RESULT, ZIP, service_time),
		increment = potraf:get_params_timestamp(?RESULT, ZIP, increment),
		post_windows_count = potraf:get_params_timestamp(?RESULT, ZIP, post_windows_count),
		package_windows_count = potraf:get_params_timestamp(?RESULT, ZIP, package_windows_count)}.

add_traffic_info(ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf:add(?RAW_DATA, ZIP, Id, Val) end,
	    record_to_tuplelist(traffic, Traf_info)).

add_timestamps_info(ZIP, Timestamp) ->
    Fields = record_info(fields, traffic),
    foreach(fun(Field) -> potraf:add_params_timestamp(?RAW_DATA, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Traf_info, Timestamp) ->
    foreach(fun({Id, Val}) -> potraf:set_last_timestamps(?RESULT, ZIP, Id, Timestamp) end,
	    filter(fun({_, Val})-> potraf:is_useful(Val) end, 
		   record_to_tuplelist(traffic, Traf_info))).

add_traffic_and_timestamps_info(ZIP, Traf_info) ->
    add_traffic_info(ZIP, Traf_info),
    Timestamp = now(),
    add_timestamps_info(ZIP, Timestamp),
    set_last_timestamps(ZIP, Traf_info, Timestamp).

get_data_status(ZIP) ->
    case get_updating_status() of
	ready -> ready;
	_ -> check_need_update(ZIP)
    end.

get_updating_status() ->
    {reply, Reply} = gen_server:call(?UPDATER, #data_req{request = info, info_type = updating_status}),
    Reply.

check_need_update(ZIP) ->
    {reply, Reply} = gen_server:call(?UPDATER, #data_req{request = info, info_type = zip_status, param = ZIP}),
    Reply.
	    
wait_data_ready(ZIP) ->
    case get_data_status(ZIP) of
	ready -> ready;
	_ -> wait_data_ready(ZIP)
    end.
	  
    

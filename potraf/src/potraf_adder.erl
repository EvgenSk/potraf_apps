
-module(potraf_adder).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include_lib("definitions.hrl").

-import(lists, [foreach/2, filter/2]).

%% 
%% API
%% 

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(ServerName) ->
    gen_server:start_link(ServerName, ?MODULE, [], []).

init(_Args) ->
    {ok, ready}.

%% handle_call

handle_call(_Req, _Msg, State) ->
    {reply, ok, State}.

%% handle_cast

handle_cast(#potraf_req{request = add, param = {ZIP, Traf_info, Timestamp}}, State) ->
    add_traffic_and_timestamps_info(ZIP, Traf_info, Timestamp),
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

add_traffic_info(ZIP, Traf_info) ->
    potraf_lib:run_with_connection(fun(Connection)-> 
					   add_traffic_info(Connection, ZIP, Traf_info) end, 
				   ?RAW_DATA).

add_traffic_info(Connection, ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf_lib:add(Connection, ZIP, Id, Val) end,
	    Traf_info).

add_timestamps_info(ZIP, Fields, Timestamp) ->
    potraf_lib:run_with_connection(fun(Connection)-> 
					   add_timestamps_info(Connection, ZIP, Fields, Timestamp) end, 
				   ?RAW_DATA).

add_timestamps_info(Connection, ZIP, Fields, Timestamp) ->
    foreach(fun(Field) -> potraf_lib:add_params_timestamp(Connection, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Fields, Timestamp) ->
    potraf_lib:run_with_connection(fun(Connection)-> 
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
    potraf_lib:run_with_connection(fun(Connection)-> 
					   potraf_lib:mark_for_upd(Connection, Updating_key, ZIP) end,
				   ?RESULT).

add_traffic_and_timestamps_info(ZIP, Traf_info, Timestamp) ->
    Useful_elems = get_useful_elems(Traf_info),
    if length(Useful_elems) > 0 ->
	    mark_for_upd(ZIP),
	    add_traffic_info(ZIP, Useful_elems),
	    Useful_fields = lists:map(fun({Id, _Val})-> Id end, Useful_elems),
	    add_timestamps_info(ZIP, Useful_fields, Timestamp),
	    set_last_timestamps(ZIP, Useful_fields, Timestamp)
    end.

get_useful_elems(Traf_info) ->
    filter(fun({_, Val})-> potraf_lib:is_useful(Val) end, 
	   ?record_to_tuplelist(traffic, Traf_info)).

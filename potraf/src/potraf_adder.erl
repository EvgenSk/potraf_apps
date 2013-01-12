
-module(potraf_adder).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([request/1, request/2]).
-export([async_request/1, async_request/2]).

-include("definitions.hrl").

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

request(Client, Req) ->
    gen_server:call(Client, Req).

request(Req) ->
    {ok, Client} = start_link(),
    request(Client, Req).

async_request(Client, Req) ->
    gen_server:cast(Client, Req).

async_request(Req) ->
    {ok, Client} = start_link(),
    async_request(Client, Req).

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
    add_traffic_info(potraf_lib:get_connection(?RAW_DATA), ZIP, Traf_info).

add_traffic_info(Connection, ZIP, Traf_info) ->
    foreach(fun({Id, Val}) -> potraf_lib:add(Connection, ZIP, Id, Val) end,
	    ?record_to_tuplelist(traffic, Traf_info)).

add_timestamps_info(ZIP, Timestamp) ->
    add_timestamps_info(potraf_lib:get_connection(?RAW_DATA), ZIP, Timestamp).

add_timestamps_info(Connection, ZIP, Timestamp) ->
    Fields = record_info(fields, traffic),
    foreach(fun(Field) -> potraf_lib:add_params_timestamp(Connection, ZIP, Field, Timestamp) end, 
	    Fields).

set_last_timestamps(ZIP, Traf_info, Timestamp) ->
    set_last_timestamps(potraf_lib:get_connection(?RAW_DATA), ZIP, Traf_info, Timestamp).

set_last_timestamps(Connection, ZIP, Traf_info, Timestamp) -> 
    foreach(fun({Id, _Val}) -> potraf_lib:set_last_timestamp(Connection, ZIP, Id, Timestamp) end,
	    filter(fun({_, Val})-> potraf_lib:is_useful(Val) end, 
		   ?record_to_tuplelist(traffic, Traf_info))).

mark_for_upd(ZIP) ->
    Updating_key = 
	case informer:get_updating_status() of
	    ready -> main;
	    _ -> tmp
	end,
    potraf_lib:mark_for_upd(potraf_lib:get_connection(?RESULT), Updating_key, ZIP).

add_traffic_and_timestamps_info(ZIP, Traf_info, Timestamp) ->
    mark_for_upd(ZIP),
    add_traffic_info(ZIP, Traf_info),
    add_timestamps_info(ZIP, Timestamp),
    set_last_timestamps(ZIP, Traf_info, Timestamp).

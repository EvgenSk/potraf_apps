
-module(potraf_getter).
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

handle_call(#potraf_req{request = get, param = ZIP}, From, State) ->
    case get_data_status(ZIP) of
	ready -> 
	    Traffic = get_traffic_info(ZIP),
	    Timestamps = get_timestamps(ZIP),
	    {stop, normal, {Traffic, Timestamps}, State}; % TODO: may be we need record for reply {Traffic, Timestamps}
	_ -> 
	    gen_event:add_handler(?UPD_EVENT_MGR, update_event_handler, [From, ZIP]),
	    {noreply, State, 500}
    end.

%% handle_cast

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info

handle_info(timeout, State) ->
    {stop, normal, State};

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

get_traffic_info(ZIP) ->
    potraf_lib:get_traffic_info(potraf_lib:get_connection(?RESULT), ZIP).

get_timestamps(ZIP) ->
    potraf_lib:get_timestamps(potraf_lib:get_connection(?RESULT), ZIP).

get_data_status(ZIP) ->
    case informer:get_updating_status() of
	#data_status{status = up_to_date} -> ready;
	_ -> check_need_update(ZIP)
    end.

check_need_update(ZIP) ->
    potraf_lib:check_need_upd(potraf_lib:get_connection(?RESULT), ZIP).


-module(informer).
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
    timer:send_interval(Time_interval, #data_req{request = update}), % may be we must use apply_interval instead of send_interval
    case Update_now of
	update -> 
	    gen_server:cast(self(), #data_req{request = update}),
	    {ok, #data_status{status = updating}};
	_ -> 
	    {ok, #data_status{status = up_to_date}}
    end.


%% handle_cast

handle_cast(#data_req{request = update}, #data_status{status = up_to_date}) ->
    gen_server:cast(?UPDATER, #data_req{request = update}),
    {noreply, #data_status{status = updating}};

handle_cast(#data_req{request = updating_finished}, _State) ->
    {noreply, #data_status{status = up_to_date}}.

handle_cast(_Msg, State)
  when State = #data_status{status = updating} ->
    {noreply, State}.

%% handle_call
    
handle_call(#data_req{request = update}, _From, #data_status{status = up_to_date}) ->
    gen_server:cast(?UPDATER, #data_req{request = update}),
    {noreply, #data_status{status = updating}};

handle_call(#data_req{request = update}, _From, State) 
  when State = #data_status{status = updating} ->
    {noreply, State};

handle_call(#data_req{request = info, info_type = updating_status}, _From, State) ->
    {reply, State, State};

handle_call(#data_req{request = info, info_type = zip_status, param = ZIP}, _From, State) ->
    if
	State == #data_status{status = updating} ; potraf_lib:check_need_update(potraf_client:get_connection(?RESULT), ZIP) -> 
	    #data_status{status = updating};
	_ -> #data_status{status = up_to_date}
    end.

%%  handle_info

handle_info(#data_req{request = update}, #data_status{status = up_to_date}) ->
    gen_server:cast(?UPDATER, #data_req{request = update}),
    {noreply, #data_status{status = updating}};

handle_info(_Msg, State)
  when State = #data_status{status = updating} ->
    {noreply, State}.


-module(updater).
-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-import(lists, [map/2, foreach/2]).

-include("definitions.hrl").

%% 
%% API (gen_server functions)
%% 

start_link(Update_now) ->
    gen_server:start_link(?MODULE, [Update_now], []).

start_link(ServerName, Update_now) ->
    gen_server:start_link(ServerName, ?MODULE, [Update_now], []).

init([update]) ->
    update_data(),
    {ok, #data_status{status = up_to_date}};

init(_Args) ->
    {ok, #data_status{status = up_to_date}}.

%% handle_call

handle_call(Req, _From, State) ->
    gen_server:cast(self(), Req),
    {reply, ok, State}.

%% handle_cast

handle_cast(#data_req{request = update}, _State) ->
    update_data(),
    {noreply, #data_status{status = up_to_date}};

handle_cast(_Req, State) ->
    {noreply, State}.

%% handle_info

handle_info(Info, State) ->
    gen_server:cast(self(), Info),
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

update_data() ->
    Connection = potraf_client:get_connection(result),
    update_each_zip(Connection),
    potraf_lib:swap_upd_zips(Connection),
    gen_server:cast(?INFORMER, #data_req{request = updating_finished}).

update_each_zip(Connection) ->
    ZIP = potraf_lib:get_zip_for_upd(Connection),
    case ZIP of
	undefined -> ok;
	_ -> 
	    Avg_vals = map(fun(Param) -> 
				   {Param, 
				    potraf_lib:get_average_for_time(Connection, {minute, 5}, ZIP, Param)} end, % {minute, 5} must be somethiing else for increment
			   record_info(fields, traffic)),
	    foreach(fun({Id, Val}) -> potraf_lib:set(Connection, ZIP, Id, Val) end, 
		    Avg_vals),
	    update_each_zip(Connection)
    end.
    

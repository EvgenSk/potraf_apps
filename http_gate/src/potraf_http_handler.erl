
-module(potraf_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include_lib("potraf/include/definitions.hrl").

%% 
%% API
%% 

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    Potraf_req = get_potraf_req(Method, Req2),
    Potraf_repl = send_potraf_req(Potraf_req),
    Repl = wrap_potraf_repl(Potraf_repl),
    {ok, Repl, State}.

terminate(_Req, _State) ->
    ok.

%% 
%% Internal functions
%% 

get_potraf_req(<<"GET">>, Req) ->
    {QSVals_bin, Req2} = cowboy_req:qs_vals(),
    QSVals = map(fun({Key, Val})-> {binary_to_atom(Key), 
				    binary_to_list(Val)}
		 end, 
		 QSVals_bin),
    case proplists:get_value(request) of
	get -> get_req(ZIP);
	add -> add_req(ZIP, QSVals)	    
    end.
    
get_req(ZIP) ->
    #potraf_req{request = get,
		param = ZIP}.

add_req(ZIP, QSVals) ->
    #potraf_req{request = add,
		param = 
		    {ZIP,
		    #traffic{
		       people_count = proplists:get_value(people_count),
		       service_time = proplists:get_value(service_time),
		       post_windows_count = proplists:get_value(post_windows_count),
		       package_windows_count = proplists:get_value(package_windows_count)}}}.

send_potraf_req(Req)
  when Req = #potraf_req{request = get, param = ZIP} ->
    {ok, Client} = potraf_client:start_link(),
    %% may ne add Client to potraf_sup
    gen_server:call(Client, Req);

send_potraf_req(Req) 
  when #potraf_req{request = add} = Req ->
    {ok, Client} = potraf:start_link(),
    %% may ne add Client to potraf_sup
    gen_server:cast(Client, Req),
    ok.

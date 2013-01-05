
-module(potraf_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include_lib("potraf/include/definitions.hrl").

%% 
%% API
%% 

init(_Transport, Req, [Repl_type]) ->
    {ok, Req, Repl_type}.

handle(Req, Repl_type) ->
    {Method, Req2} = cowboy_req:method(Req),
    Potraf_req = get_potraf_req(Method, Req2),
    Potraf_repl = send_potraf_req(Potraf_req),
    {ok, Req3} = send_response(Repl_type, Potraf_repl, Req2),
    {ok, Req3, State}.

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
    ok;

send_potraf_req(_Req) ->
    undefined.

send_response(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing request parameters.">>);

send_response(ok, Req) ->
    cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], "ok", Req);

send_response(Reply_type, Reply, Req) ->
    Wrapped_repl = wrap_potraf_repl(Reply_type, Reply),
    cowboy_req:reply(200, 
		     [{<<"content-encoding">>, <<"utf-8">>}], 
		     Wrapped_repl, 
		     Req).

wrap_potraf_repl(simple, {Traf, Time}) ->
    string:join([to_KeyVal_list_string(Traf), 
		 to_KeyVal_list_string(Traf)], 
		"|").

to_KeyVal_list_string(Rec) ->
    KeyVal_pairs = 
	map(fun({Id, Val}) -> 
		    string:join([atom_to_list(Id), float_to_list(Val)], ":") 
	    end,
	    record_to_tuplelist(traffic, Rec)),
    string:join(KeyVal_pairs, ";").

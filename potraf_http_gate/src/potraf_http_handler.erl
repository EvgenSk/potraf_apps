
-module(potraf_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include_lib("definitions.hrl").

-import(lists, [map/2]).

%% 
%% API
%% 

init(_Transport, Req, [Repl_type]) ->
    {ok, Req, Repl_type}.

handle(Req, Repl_type) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Potraf_req, Req3} = get_potraf_req(Method, Req2),
    Potraf_repl = send_potraf_req(Potraf_req),
    {ok, Req4} = send_response(Repl_type, Potraf_repl, Req3),
    {ok, Req4, Repl_type}.

terminate(_Req, _State) ->
    ok.

%% 
%% Internal functions
%% 

get_potraf_req(<<"GET">>, Req) ->
    {QSVals_bin, Req2} = cowboy_req:qs_vals(Req),
    QSVals = map(fun({Key, Val})-> {binary_to_atom(Key, latin1), 
				    binary_to_list(Val)}
		 end, 
		 QSVals_bin),
    case proplists:get_value(request) of
	get -> {get_req(proplists:get_value(zip, QSVals)), Req2};
	add -> {add_req(QSVals), Req2}	        
    end.
    
get_req(ZIP) ->
    #potraf_req{request = get,
		param = ZIP}.

add_req(QSVals) ->
    #potraf_req{request = add,
		param = 
		    {proplists:get_value(zip, QSVals),
		    #traffic{
		       people_count = proplists:get_value(people_count, QSVals),
		       service_time = proplists:get_value(service_time, QSVals),
		       post_windows_count = proplists:get_value(post_windows_count, QSVals),
		       package_windows_count = proplists:get_value(package_windows_count, QSVals)}}}.

send_potraf_req(Req = #potraf_req{request = get}) ->
    {ok, Client} = potraf_client:start_link(),
    %% may ne add Client to potraf_sup
    gen_server:call(Client, Req);

send_potraf_req(Req = #potraf_req{request = add}) ->
    {ok, Client} = potraf:start_link(),
    %% may ne add Client to potraf_sup
    gen_server:cast(Client, Req),
    ok;

send_potraf_req(_Req) ->
    undefined.

send_response(_Repl_type, undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing request parameters.">>, Req);

send_response(_Repl_type, ok, Req) ->
    cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], "ok", Req);

send_response(Reply_type, Reply, Req) ->
    Wrapped_repl = wrap_potraf_repl(Reply_type, Reply),
    cowboy_req:reply(200, 
		     [{<<"content-encoding">>, <<"utf-8">>}], 
		     Wrapped_repl, 
		     Req).

wrap_potraf_repl(simple, {Traf, Time}) ->
    string:join([to_KeyVal_list_string(Traf), 
		 to_KeyVal_list_string(Time)], 
		"|").

to_KeyVal_list_string(Rec) ->
    KeyVal_pairs = 
	map(fun({Id, Val}) -> 
		    string:join([atom_to_list(Id), float_to_list(Val)], ":") 
	    end,
	    ?record_to_tuplelist(traffic, Rec)),
    string:join(KeyVal_pairs, ";").

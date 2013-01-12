
-module(potraf_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-include_lib("definitions.hrl").

-import(lists, [map/2, zip/2]).

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
    case cowboy_req:qs_val(<<"request">>, Req) of
    	{<<"get">>, Req2} -> {get_req(get, Req2), Req2};
    	{<<"add">>, Req2} -> {add_req(get, Req2), Req2}
    end;

get_potraf_req(<<"POST">>, Req) ->
    case cowboy_req:has_body(Req) of
	{true, Req2} -> {fetch_req(Req2), Req2};
	{false, Req2} -> {ok, Req2}
    end.
    
%% for GET HTTP-requests

get_req(get, Req) ->
    {ZIP_bin, _Req2} = cowboy_req:qs_val(<<"zip">>, Req),
    ZIP = list_to_integer(binary_to_list(ZIP_bin)),
    #potraf_req{request = get,
		param = ZIP};

get_req(post, PostVals) ->
    ZIP_bin = proplists:get_value(<<"zip">>, PostVals),
    ZIP = list_to_integer(binary_to_list(ZIP_bin)),
    #potraf_req{request = get,
		param = ZIP}.

add_req(get, Req) ->
    get_potraf_req_by_func(fun(Id) -> {Val, _} = cowboy_req:qs_val(atom_to_binary(Id, latin1), Req), 
				      Val end);

add_req(post, PostVals) ->
    get_potraf_req_by_func(fun(Id) -> proplists:get_value(atom_to_binary(Id, latin1), PostVals) end).

%% for POST HTTP-requests

fetch_req(Req) ->
    {ok, PostVals, _} = cowboy_req:body_qs(Req),
    case proplists:get_value(<<"request">>, PostVals) of
	<<"get">> -> get_req(post, PostVals);
	<<"add">> -> add_req(post, PostVals)
    end.
    
get_potraf_req_by_func(Func) ->
    Vals = map(fun(Val) -> qs_val_to_int(Val) end,
	       map(fun(Id)-> Func(Id) end,
		   record_info(fields, traffic))),
    KeyVals = lists:zip(record_info(fields, traffic), Vals),
    ZIP_bin = Func(zip),
    ZIP = list_to_integer(binary_to_list(ZIP_bin)),
    #potraf_req{request = add,
		param = 
		    {ZIP,
		    #traffic{
		       people_count = proplists:get_value(people_count, KeyVals),
		       service_time = proplists:get_value(service_time, KeyVals),
		       post_windows_count = proplists:get_value(post_windows_count, KeyVals),
		       package_windows_count = proplists:get_value(package_windows_count, KeyVals)}}}.    

send_potraf_req(Req) ->
    potraf:request(Req).

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
		    string:join([atom_to_list(Id), Val], ":") 
	    end,
	    ?record_to_tuplelist(traffic, Rec)),
    string:join(KeyVal_pairs, ";").

qs_val_to_int(QSVal) ->
    case QSVal of
	undefined -> undefined; 
	_ -> list_to_integer(binary_to_list(QSVal)) 
    end.


-module(potraf_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-import(lists, [map/2, zip/2]).

%% 
%% API
%% 

init(_Transport, Req, [Repl_type]) ->
    {ok, Req, Repl_type}.

handle(Req, Repl_type) ->
    {Method, Req2} = cowboy_req:method(Req),
    Potraf_req = get_potraf_req(Method, Req2),
    Potraf_repl = send_potraf_req(Potraf_req),
    {ok, Req4} = send_response(Repl_type, Potraf_repl, Req2),
    {ok, Req4, Repl_type}.

terminate(_Req, _State) ->
    ok.

%% 
%% Internal functions
%% 

get_potraf_req(<<"GET">>, Req) ->
    {get, get_req(Req)};

get_potraf_req(<<"POST">>, Req) ->
    case cowboy_req:has_body(Req) of
	{true, _Req2} -> {add, add_req(Req)};
	{false, _Req2} -> ok
    end;

get_potraf_req(_Method, _Req) ->
    not_implemented.
    
%% for GET HTTP-requests

get_req(Req) ->
    fetch_zip(Req).

add_req(Req) ->
    ZIP = fetch_zip(Req),
    {ok, PostVals, _} = cowboy_req:body_qs(Req),    
    KeyVals = map(fun({Key, Val})-> {binary_to_atom(Key, latin1), qs_val_to_int(Val)}end,
		  map(fun(Key)-> {Key, proplists:get_value(Key, PostVals)} end,
		      proplists:get_keys(PostVals))),
    {ZIP, KeyVals}.

fetch_zip(Req) ->
    {ZIP_bin, _Req2} = cowboy_req:qs_val(<<"zip">>, Req),
    list_to_integer(binary_to_list(ZIP_bin)).

send_potraf_req({get, ZIP})->
    potraf:request(get, ZIP);

send_potraf_req({add, Params})->
    potraf:request(add, Params);

send_potraf_req(Anything_else)->
    Anything_else.

send_response(_Repl_type, undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing request parameters.">>, Req);

send_response(_Repl_type, ok, Req) ->
    cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], "ok", Req);

send_response(_Repl_type, not_implemented, Req) ->
    cowboy_req:reply(501, [], <<"Only GET and POST methods are implemented.">>, Req);

send_response(Reply_type, Reply, Req) ->
    Wrapped_repl = wrap_potraf_repl(Reply_type, Reply),
    cowboy_req:reply(200, 
		     [{<<"content-encoding">>, <<"utf-8">>}], 
		     Wrapped_repl, 
		     Req).

wrap_potraf_repl(simple, Potraf_repl) ->
    KeyValTime = map(fun({Key, Val, Time})-> 
			     string:join([utils:to_list(Key), Val, Time], ":") end,
		     Potraf_repl),
    string:join(KeyValTime, ";").

qs_val_to_int(QSVal) ->
    case QSVal of
	undefined -> undefined; 
	_ -> list_to_integer(binary_to_list(QSVal)) 
    end.

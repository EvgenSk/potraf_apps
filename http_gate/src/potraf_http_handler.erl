
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
    {ok, Potraf_repl, State}.

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
    case proplists:get_value(type) of
	get -> get_info(ZIP);
	add -> add_info(ZIP, QSVals)	    
    end.
    
get_info(ZIP) ->
    %% create potraf_req,
    %% create potraf_client,
    %% request info from it
    ok.

add_info(ZIP, QSVals) ->
    %% create potraf_req,
    %% create potraf_client,
    %% request add info
    ok.

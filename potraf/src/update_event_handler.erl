
-module(update_event_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%% behaviour callbacks

init([From, ZIP]) ->
    {ok, {From, ZIP}}.

handle_event({updated, ZIP, Data}, {From, ZIP}) ->
    gen_server:reply(From, Data),
    remove_handler;

handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

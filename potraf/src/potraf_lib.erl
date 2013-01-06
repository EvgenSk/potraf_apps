-module(potraf_lib).

-export([to_string/1]).
-export([get/3]).
-export([add/4]).
-export([get_last_n/4]).
-export([bin_to_num/1]).
-export([get_params_timestamp/3]).
-export([set_params_timestamp/4]).
-export([add_params_timestamp/4]).
-export([get_connection/1]).
-export([get_by_string/2]).
-export([add_by_string/3]).
-export([set_by_string/3]).
-export([set/4]).
-export([get_actual_count/5]).
-export([get_average_for_time/4]).
-export([add_actual_suff/1]).
-export([get_last_timestamp/3]).
-export([set_last_timestamp/4]).
-export([get_zip_for_upd/1]).
-export([get_update_key/1]).
-export([mark_for_upd/3]).
-export([swap_upd_zips/1]).
-export([check_need_upd/2]).
-export([is_useful/1]).

%%-include("../include/eredis.hrl").

-import(lists, [map/2, sum/1]).

%% 
%% Internal functions
%% 

to_string(Some) ->
    lists:flatten(io_lib:format("~p", [Some])).

get_q_string(ZIP, Param) ->
    case Param of
	people_count -> lists:concat([to_string(ZIP), ":", "people-count"]);
	service_time -> lists:concat([to_string(ZIP), ":", "service-time"]);
	increment -> lists:concat([to_string(ZIP), ":", "increment"]);
	post_windows_count -> lists:concat([to_string(ZIP), ":", "post-windows-count"]);
	package_windows_count -> lists:concat([to_string(ZIP), ":", "package-windows-count"])
    end.

get_timestamp_q_strings(ZIP, Param) ->
    Timestamp_param = 
	case Param of
	    res_timestamp -> lists:concat([to_string(ZIP), ":", "res-timestamp"]);
	    _ -> lists:concat(get_q_string(ZIP, Param), ":", "timestamp")
	end,
    {lists:concat(Timestamp_param, ":", "mega"),
     lists:concat(Timestamp_param, ":", "second")}.

get_params_timestamp(Connection, ZIP, Param) ->
    {Mega, Second} = get_timestamp_q_strings(ZIP, Param),
    {get_by_string(Connection, Mega),
     get_by_string(Connection, Second)}.

set_params_timestamp(Connection, ZIP, Param, {Mega, Second}) ->
    {Mega_str, Second_str} = get_timestamp_q_strings(ZIP, Param),
    set_by_string(Connection, Mega_str, Mega),
    set_by_string(Connection, Second_str, Second).

add_params_timestamp(Connection, ZIP, Param, Timestamp) ->
    {Mega_str, Second_str} = get_timestamp_q_strings(ZIP, Param),
    {Mega_val, Second_val, _} = Timestamp,
    add_by_string(Connection, Mega_str, Mega_val),
    add_by_string(Connection, Second_str, Second_val).

get_connection(Num) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", Num]),
    C.

get(Connection, ZIP, Param) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["GET", Q_string]).

get_by_string(Connection, Q_string) ->
    eredis:q(Connection, ["GET", Q_string]).

add(Connection, ZIP, Param, Value)->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["LPUSH", Q_string, Value]).

add_by_string(Connection, Q_string, Value) ->
    eredis:q(Connection, ["LPUSH", Q_string, Value]).
    
set_by_string(Connection, Q_string, Value) ->
    eredis:q(Connection, ["SET", Q_string, Value]).
 
set(Connection, ZIP, Param, Value) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["SET", Q_string, Value]).

get_last_n(Connection, ZIP, Param, N) ->
    {Mega, _} = get_timestamp_q_strings(ZIP, Param),
    eredis:q(Connection, ["LRANGE", Mega, 0, N - 1]).

get_actual_count(Connection, {Time_int, Count}, ZIP, Param, MAX) -> 
    {Mega, Seconds, _} = now(),				   % may be use micro?
    Cur_time = Mega * 1000000 + Seconds,
    Times = get_last_n(Connection, ZIP, Param, MAX),
    Coeff = 
	case Time_int of
	    second -> 1;
	    minute -> 60;
	    hour -> 3600
	end,
    Min_time = Cur_time - Coeff * Count,
    Actual_times = lists:filter(fun(Time) -> Time > Min_time end, Times),
    length(Actual_times).

max_useful() ->
    100.

is_useful(Data) ->
    Data /= unuseful_elem().

unuseful_elem() ->
    -1.

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

get_average_for_time(Connection, {Time_int, Count}, ZIP, Param) ->
    N = get_actual_count(Connection, {Time_int, Count}, ZIP, Param, max_useful()),
    Data = map(fun(Elem) -> bin_to_num(Elem) end, 
	       get_last_n(Connection, ZIP, Param, N)),
    Useful_data = lists:filter(fun(Elem) -> Elem /= unuseful_elem() end, 
			       Data),
    Useful_length = length(Useful_data),
    case Useful_length of
	0 -> -1;
	_ -> sum(Useful_data) / Useful_length
    end.
	    
add_actual_suff(Param) ->
    lists:concat(Param, ":", "actual").

get_last_timestamp(Connection, ZIP, Param) ->
    {Mega, Seconds} = get_timestamp_q_strings(ZIP, Param),
    {get_by_string(Connection, add_actual_suff(Mega)),
     get_by_string(Connection, add_actual_suff(Seconds))}.

set_last_timestamp(Connection, ZIP, Param, Timestamp) ->
    {Mega_str, Seconds_str} = get_timestamp_q_strings(ZIP, Param),
    {Mega_val, Seconds_val} = Timestamp,
    {set_by_string(Connection, add_actual_suff(Mega_str), Mega_val),
     set_by_string(Connection, add_actual_suff(Seconds_str), Seconds_val)}.

get_zip_for_upd(Connection) ->
    eredis:q(Connection, ["SPOP", get_update_key(main)]).

get_update_key(main) ->
    "need-update";

get_update_key(tmp) ->
    "need-update-tmp".

mark_for_upd(Connection, Key, ZIP) ->
    eredis:q(Connection, ["SADD", Key, ZIP]).

swap_upd_zips(Connection) ->
    eredis:q(Connection, ["SUNIONSTORE", get_update_key(main), get_update_key(tmp)]),
    eredis:q(Connection, ["DEL", get_update_key(tmp)]).

check_need_upd(Connection, ZIP) ->
    {ok, Bin_res} = eredis:q(Connection, ["SISMEMBER", get_update_key(main), ZIP]),
    case bin_to_num(Bin_res) of
	1 -> ready;
	0 -> updating
    end.
	    

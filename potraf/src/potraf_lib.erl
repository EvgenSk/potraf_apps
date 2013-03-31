-module(potraf_lib).

-export([get/3]).
-export([add/4]).
-export([get_last_n/4]).
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
-export([unmark_for_upd/2]).
-export([swap_upd_zips/1]).
-export([check_need_upd/2]).
-export([is_useful/1]).
-export([set_result_timestamp/3]).
-export([get_timestamps/2]).
-export([get_traffic_info/2]).
-export([close_connection/1]).
-export([(run_with_connection/2)]).
-export([max_useful/0]).
-export([trim/4]).
-export([trim/3]).
-export([upd_time_interval/0]).
-export([set_expiration_time/4]).

-include_lib("definitions.hrl").

-import(lists, [map/2, sum/1]).
-import(utils, [bin_to_num/1, timestamp_to_list/1, to_int_or_atom/1, to_list/1]).

%% 
%% Internal functions
%% 

get_connection(result) ->
    get_connection(0);

get_connection(raw_data) ->
    get_connection(1);

get_connection(Num) ->
    {ok, C} = eredis:start_link([{database, Num}, 
				 {reconnect_sleep, no_reconnect}]),
    C.

close_connection(Connection) ->
    eredis:q(Connection, ["QUIT"]).

%% opens connection, runs Fun with it and closes connection
run_with_connection(Fun, Connection_name) ->
    Connection = get_connection(Connection_name),
    Result = Fun(Connection),
    close_connection(Connection),
    Result.

%% makes key-string for ZIP and Param 
get_q_string(ZIP, Param) ->
    case Param of
	people_count -> lists:concat([to_list(ZIP), ":", "people-count"]);
	service_time -> lists:concat([to_list(ZIP), ":", "service-time"]);
	post_windows_count -> lists:concat([to_list(ZIP), ":", "post-windows-count"]);
	package_windows_count -> lists:concat([to_list(ZIP), ":", "package-windows-count"])
    end.

%% makes key-strings for timestamp for ZIP and Param
get_timestamp_q_strings(ZIP, Param) ->
    Timestamp_param = 
	case Param of
	    res_timestamp -> lists:concat([to_list(ZIP), ":", "res-timestamp"]);
	    _ -> lists:concat([get_q_string(ZIP, Param), ":", "timestamp"])
	end,
    {lists:concat([Timestamp_param, ":", "mega"]),
     lists:concat([Timestamp_param, ":", "second"])}.

get_params_timestamp(Connection, ZIP, Param) ->
    {Mega, Second} = get_timestamp_q_strings(ZIP, Param),
    timestamp_to_list({to_int_or_atom(get_by_string(Connection, Mega)),
		       to_int_or_atom(get_by_string(Connection, Second))}).

set_params_timestamp(Connection, ZIP, Param, {Mega, Second}) ->
    {Mega_str, Second_str} = get_timestamp_q_strings(ZIP, Param),
    set_by_string(Connection, Mega_str, Mega),
    set_by_string(Connection, Second_str, Second).

add_params_timestamp(Connection, ZIP, Param, Timestamp) ->
    {Mega_str, Second_str} = get_timestamp_q_strings(ZIP, Param),
    {Mega_val, Second_val, _} = Timestamp,
    add_by_string(Connection, Mega_str, Mega_val),
    add_by_string(Connection, Second_str, Second_val).

set_result_timestamp(Connection, ZIP, Timestamp) ->
    {Mega, Second, _} = Timestamp,
    {Mega_str, Second_str} = get_timestamp_q_strings(ZIP, res_timestamp),
    set_by_string(Connection, Mega_str, Mega),
    set_by_string(Connection, Second_str, Second).
    
get(Connection, ZIP, Param) ->
    Q_string = get_q_string(ZIP, Param),
    {ok, Val} = eredis:q(Connection, ["GET", Q_string]),
    Val.

get_by_string(Connection, Q_string) ->
    {ok, Val} = eredis:q(Connection, ["GET", Q_string]),
    Val.


add(Connection, ZIP, Param, Value)->
    Q_string = get_q_string(ZIP, Param),
    {ok, Val} = eredis:q(Connection, ["LPUSH", Q_string, to_list(Value)]),
    Val.

add_by_string(Connection, Q_string, Value) ->
    {ok, Val} = eredis:q(Connection, ["LPUSH", Q_string, to_list(Value)]),
    Val.
    
set_by_string(Connection, Q_string, Value) ->
    {ok, Val} = eredis:q(Connection, ["SET", Q_string, to_list(Value)]),
    Val.
 
set(Connection, ZIP, Param, Value) ->
    Q_string = get_q_string(ZIP, Param),
    {ok, Val} = eredis:q(Connection, ["SET", Q_string, to_list(Value)]),
    Val.

get_last_n_times(Connection, ZIP, Param, N) ->
    {Mega, Second} = get_timestamp_q_strings(ZIP, Param),
    {ok, MegaVals} = eredis:q(Connection, ["LRANGE", Mega, 0, N - 1]),
    {ok, SecondVals} = eredis:q(Connection, ["LRANGE", Second, 0, N - 1]),
    case MegaVals of
	[] -> [];
	_ -> 
	    map(fun(TimestampList)-> list_to_integer(TimestampList) end,
		map(fun({MBin, SBin})-> utils:timestamp_to_list({MBin, SBin}) end,
		    lists:zip(MegaVals, SecondVals)))
    end.

get_actual_count(Connection, Time_interval, ZIP, Param, MAX) -> 
    {Mega, Seconds, _} = now(),				   % may be use micro?
    Cur_time = Mega * 1000000 + Seconds,
    Times = get_last_n_times(Connection, ZIP, Param, MAX),
    Min_time = Cur_time - Time_interval,
    Actual_times = lists:filter(fun(Time) -> Time > Min_time end, Times),
    length(Actual_times).

max_useful() ->
    case application:get_env(max_useful) of
	{ok, Max_useful} -> Max_useful;
	_ -> 50
    end.

is_useful(undefined) ->
    false;

is_useful(Data) ->
    Data >= 0.

get_average_for_time(Connection, Time_interval, ZIP, Param) ->
    N = get_actual_count(Connection, Time_interval, ZIP, Param, max_useful()),
    Data = get_last_n(Connection, ZIP, Param, N),
    Useful_data = lists:filter(fun(Elem) -> is_useful(Elem) end, 
			       Data),
    Useful_length = length(Useful_data),
    case Useful_length of
	0 -> undefined;
	_ -> sum(Useful_data) / Useful_length
    end.
	    
get_last_n(Connection, ZIP, Param, N) ->
    case N of 
	0 -> [];
	_ -> 
	    Q_string = get_q_string(ZIP, Param),
	    {ok, Vals} = eredis:q(Connection, ["LRANGE", Q_string, 0, N - 1]),
	    case Vals of
		[] -> [];
		_ -> map(fun(Val)-> utils:to_int_or_atom(Val) end, Vals)
	    end
    end.

%% adds suffix ':actual' to key-string
add_actual_suff(Param) ->
    lists:concat([Param, ":", "actual"]).

get_last_timestamp(Connection, ZIP, Param) ->
    {Mega, Seconds} = get_timestamp_q_strings(ZIP, Param),
    {get_by_string(Connection, add_actual_suff(Mega)),
     get_by_string(Connection, add_actual_suff(Seconds))}.

set_last_timestamp(Connection, ZIP, Param, Timestamp) ->
    {Mega_str, Seconds_str} = get_timestamp_q_strings(ZIP, Param),
    {Mega_val, Seconds_val, _} = Timestamp,
    {set_by_string(Connection, add_actual_suff(Mega_str), Mega_val),
     set_by_string(Connection, add_actual_suff(Seconds_str), Seconds_val)}.

get_timestamps(Connection, ZIP) ->
    #timestamps{people_count = timestamp_to_list(get_last_timestamp(Connection, ZIP, people_count)),
		service_time = timestamp_to_list(get_last_timestamp(Connection, ZIP, service_time)),
		post_windows_count = timestamp_to_list(get_last_timestamp(Connection, ZIP, post_windows_count)),
		package_windows_count = timestamp_to_list(get_last_timestamp(Connection, ZIP, package_windows_count))}.

get_traffic_info(Connection, ZIP) ->
    #traffic{people_count = to_list(get(Connection, ZIP, people_count)),
	     service_time = to_list(get(Connection, ZIP, service_time)),
	     post_windows_count = to_list(get(Connection, ZIP, post_windows_count)),
	     package_windows_count = to_list(get(Connection, ZIP, package_windows_count))}.

get_zip_for_upd(Connection) ->
    {ok, Val} = eredis:q(Connection, ["SRANDMEMBER", get_update_key(main)]),
    Val.

get_update_key(main) ->
    "need-update";

get_update_key(tmp) ->
    "need-update-tmp".

mark_for_upd(Connection, Key, ZIP) ->
    eredis:q(Connection, ["SADD", get_update_key(Key), ZIP]).

unmark_for_upd(Connection, ZIP) ->
    eredis:q(Connection, ["SREM", get_update_key(main), ZIP]).

%% moves ZIPs for update from tmp-key to main-key
swap_upd_zips(Connection) ->
    eredis:q(Connection, ["SUNIONSTORE", get_update_key(main), get_update_key(tmp)]),
    eredis:q(Connection, ["DEL", get_update_key(tmp)]),
    ok.

check_need_upd(Connection, ZIP) ->
    {ok, Bin_res} = eredis:q(Connection, ["SISMEMBER", get_update_key(main), ZIP]),
    case bin_to_num(Bin_res) of
	0 -> ready;
	_ -> updating
    end.

%% trims all lists for ZIP:Param to 'Trim_to'
trim(Connection, ZIP, Param, Trim_to) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["LTRIM", Q_string, 0, Trim_to - 1]),
    {Q_mega, Q_second} = get_timestamp_q_strings(ZIP, Param),
    eredis:q(Connection, ["LTRIM", Q_mega, 0, Trim_to - 1]),
    eredis:q(Connection, ["LTRIM", Q_second, 0, Trim_to - 1]).
    
%% trims all lists for ZIP:Param to max_useful
trim(Connection, ZIP, Param) ->
    trim(Connection, ZIP, Param, max_useful()).

upd_time_interval()->
    case application:get_env(upd_time_interval) of
	{ok, Time_interval} -> Time_interval;
	_ -> 300
    end.

%% sets expiration time to all keys for ZIP:Param
set_expiration_time(Connection, ZIP, Param, Expire_time) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["EXPIRE", Q_string, Expire_time]),
    {Q_mega, Q_second} = get_timestamp_q_strings(ZIP, Param),
    eredis:q(Connection, ["EXPIRE", Q_mega, Expire_time]),
    eredis:q(Connection, ["EXPIRE", Q_second, Expire_time]).

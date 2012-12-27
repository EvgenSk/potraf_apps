
-module(potraf).

-export([get_people_count/1, 
	 to_string/1, 
	 get/2, 
	 get/3,
	 add/3,
	 add/4,
	 get_last_n/3,
	 get_last_n/4, 
	 bin_to_num/1]).

-include("../include/eredis.hrl").
%-import(eredis, [start_link/0, q/2]).

to_string(Some) ->
    lists:flatten(io_lib:format("~p", [Some])).

get_people_count(Post_office_index) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["GET" , lists:concat([to_string(Post_office_index), ":", "people-count"])]). % need to know what to use instead of C (connection)

for_db(result, Fun) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", 0]),
    Fun(C);

for_db(raw, Fun) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", 1]),
    Fun(C).

get_q_string(ZIP, Param) ->
    case Param of
	people_count -> lists:concat([to_string(ZIP), ":", "people-count"]);
	service_time -> lists:concat([to_string(ZIP), ":", "service-time"]);
	increment -> lists:concat([to_string(ZIP), ":", "increment"]);
	post-windows-count -> lists:concat([to_string(ZIP), ":", "post-windows-count"]);
	package-windows-count -> lists:concat([to_string(ZIP), ":", "package-windows-count"])
    end.

get_timestamp_q_strings(ZIP, Param) ->
    Timestamp_param = 
	case Param of
	    res-timestamp -> lists:concat([to_string(ZIP), ":", "res-timestamp"]);
	    next-res-timestamp -> lists:concat([to_string(ZIP), ":", "next-res-timestamp"]);
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

get_connection(result) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", 0]),
    C;

get_connection(raw_data) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", 1]),
    C;

get_connection(calculated_data) ->
    {ok, C} = eredis:start_link(),
    eredis:q(C, ["SELECT", 2]),
    C;
    
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
    
get(ZIP, Param) ->
    get(?RESULT, ZIP, Param).

add(ZIP, Param, Value) ->
    add(?RAW_DATA, ZIP, Param, Value).

set_by_string(Connection, Q_string, Value) ->
    eredis:q(Connection, ["SET", Q_string, Value]).
 
set(Connection, ZIP, Param, Value) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["SET", Q_string, Value]).

set(ZIP, Param, Value) ->
    set(?RESULT, ZIP, Param, Value).

get_last_n(Connection, ZIP, Param, N) ->
    Q_string = get_q_string(ZIP, Param),
    eredis:q(Connection, ["LRANGE", Q_string, 0, N - 1]).

get_last_n(ZIP, Param, N) ->
    get_last_n(?RAW_DATA, ZIP, Param, N).

get_actual_count(Connection, {Time_int, Count}, ZIP, MAX) -> 
    {Mega, Seconds, _} = now(),				   % may be use micro?
    Cur_time = Mega * 1000000 + Seconds,
    Times = get_last_n(Connection, ZIP, timestamp, MAX),
    Coeff = 
	case Time_int of
	    second -> 1;
	    minute -> 60;
	    hour -> 3600
	end,
    Min_time = Cur_time - Coeff * Count,
    Actual_times = lists:filter(fun(Time) -> Time > Min_time end, Times),
    length(Actual_times).

get_actual_count({Time_int, Count}, ZIP, MAX) ->
    get_actual_count(?RAW_DATA, {Time_int, Count}, ZIP, MAX).

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
    N = get_actual_count(Connection, {Time_int, Count}, ZIP, max_useful()),
    Data = map(fun(Elem) -> bin_to_num(Elem) end, 
	       get_last_n(Connection, ZIP, Param, N)),
    Useful_data = lists:filter(fun(Elem) -> Elem /= unuseful_elem() end, 
			       Data),
    Useful_length = length(Useful_data),
    case Useful_length of
	0 -> -1;
	_ -> sum(Useful_data) / Useful_length
    end.
	    
get_average_for_time({Time_int, Count}, ZIP, Param) ->
    get_average_for_time(?RAW_DATA, {Time_int, Count}, ZIP, Param).


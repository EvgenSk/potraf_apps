%% this module provides functions for calculations

-module(calcs).

default_time_interval() ->
    {minute, 5}.

increment_time_interval() ->
    {hour, 1}.

calc(Connection, ZIP, Param) ->
    Q_string = get_q_string(ZIP, Param),
    case Param of
	increment -> get_average_for_time(Connection, increment_time_interval(), ZIP, Param);
	_ -> get_average_for_time(Connection, default_time_interval(), ZIP, Param)
    end.

calc(ZIP, Param) ->
    case Param of
	increment -> calc(get_connection(calculated_data), ZIP, Param);
	_ -> calc(get_connection(raw_data), ZIP, Param)
    end.

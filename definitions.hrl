
%% connections to databases
-define(RESULT, result).
-define(RAW_DATA, raw_data).
-define(CALCULATED_DATA, calculated_data).

%% Useless element
-define(USELESS_ELEM, -1).

%% info about traffic

-record(traffic, 
	{people_count,
	 service_time,
	 increment,
	 post_windows_count,
	 package_windows_count}).

%% Timestamps for each parameter of the record

-record(timestamps, 
	{people_count,
	 service_time,
	 increment,
	 post_windows_count,
	 package_windows_count}).

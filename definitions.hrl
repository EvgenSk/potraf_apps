
%% connections to databases
-define(RESULT, result).
-define(RAW_DATA, raw_data).
-define(CALCULATED_DATA, calculated_data).

%% names of servers

-define(POTRAF_SERV, potraf_serv).
-define(HTTP_HANDLER, http_handler).

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

%% Potraf server request

-record(potraf_req, {
	  type :: get | add | update, 
	  param :: any()
	 }).

%% Potraf server state

-record(potraf_state, {
	  readiness :: ready | updating
	 }).


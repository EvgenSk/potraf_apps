
%% connections to databases
-define(RESULT, result).
-define(RAW_DATA, raw_data).
-define(CALCULATED_DATA, calculated_data).

%% names of servers

-define(POTRAF_SERV, potraf_serv).
-define(HTTP_HANDLER, http_handler).
-define(UPDATER, updater).
-define(INFORMER, informer).
-define(UPD_EVENT_MGR, update_event_manager).

%% Useless element

-define(USELESS_ELEM, -1).

%% useful macroses

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

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
	  request :: get | add, 
	  param :: any()
	 }).

%% Potraf server state

-record(potraf_state, {
	  readiness :: ready | updating
	 }).

%% Updater records

-record(data_req, {
	  request :: update | info,
	  info_type :: updating_status | zip_status,
	  param :: any()
	 }).

-record(data_status, {
	 status :: up_to_date | updating
	 }).

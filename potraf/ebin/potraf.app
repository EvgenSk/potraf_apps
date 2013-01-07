{application, potraf,
 [{description, "POst Office TRAFfic application"},
  {vsn, "1.0.0"},
  {modules, [potraf, potraf_sup, potraf_client, potraf_lib, informer, updater]},
  {registered, [potraf, informer, updater]},
  {env, [{upd_time_interval, 300000}]},
  {applications, [
		  kernel,
		  stdlib,
		  eredis
		 ]},
  {mod, {potraf, []}}
 ]}.

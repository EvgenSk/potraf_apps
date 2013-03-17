%% HTTP gate for Post Office TRAFfic applilcation

{application, potraf_http_gate,
 [{description, "HTTP gate for Post Office TRAFfic"},
  {vsn, "0.1.0"},
  {modules, [potraf_http_handler, 
	     potraf_http_gate, 
	     potraf_http_gate_sup]},
  {registered, [potraf_http_gate]},
  {applications, [cowboy, potraf]},
  {mod, {potraf_http_gate, []}},
  {env, [
	 {repl_type, simple},
	 {nbAcceptors, 100},
	 {port, 8080}
	]}
 ]}.	      

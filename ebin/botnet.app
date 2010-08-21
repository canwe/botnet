{application, botnet,
 [
  {description, "Erlang botnet"},
  {vsn, "1.0"},
  {modules, [botnet_app, botnet, botnet_node, botnet_scheduler]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto]},
  {mod, {botnet_app, []}},
  {env, []}
 ]}.
 
 
 




{application, mod_bql,
 [{description, "mod_bql"},
  {vsn, "0.01"},
  {modules, [
  ]},
  {registered, []},
  {mod, {bql_server, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.

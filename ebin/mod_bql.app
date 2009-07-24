{application, mod_bql,
 [{description, "mod_bql"},
  {vsn, "0.01"},
  {modules, [
    mod_bql,
    mod_bql_sup,
    bql_server,
    bql_applicator
  ]},
  {registered, []},
  {mod, {mod_bql, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.

{application, rabbitmq_bql,
 [{description, "RabbitMQ Broker Query Language"},
  {vsn, "0.01"},
  {modules, [
    rabbitmq_bql,
    rabbitmq_bql_sup,
    bql_server,
    bql_applicator
  ]},
  {registered, []},
  {mod, {rabbitmq_bql, []}},
  {env, []},
  {applications, [kernel, stdlib, rabbit, amqp_client]}]}.

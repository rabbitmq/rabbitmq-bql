-module(bql_test).

-include_lib("eunit/include/eunit.hrl").

-define(debugCommands(C), ?debugFmt("Got commands: ~p~n", [C])).

create_nondurable_queue_test() ->
  [ok] = execute("create queue mynondurablequeue;"),
  [{_, Result}] = execute("select * from queues where name=mynondurablequeue and 'durable'=false;"),
  ?assertEqual(1, length(Result)),
  ok.

create_durable_queue_test() ->
  [ok] = execute("create durable queue mydurablequeue;"),
  [{_, Result}] = execute("select * from queues where name=mydurablequeue and 'durable'=true;"),
  ?assertEqual(1, length(Result)),
  ok.

drop_queue_test() ->
  [ok, ok] = execute("create queue myqueuefordropping; drop queue myqueuefordropping;"),
  [{_, Result}] = execute("select * from queues where name=myqueuefordropping;"),
  ?assertEqual(0, length(Result)),
  ok.

constrain_with_invalid_field_test() ->
  Response = execute("select * from queues where invalid_field=something;"),
  ?assertEqual(["Invalid field invalid_field specified in constraint"], Response),
  ok.

order_with_invalid_field_test() ->
  Response = execute("select * from queues order by invalid_field;"),
  ?assertEqual(["Invalid field invalid_field specified in ordering clause"], Response),
  ok.

create_non_durable_exchange_test() ->
  [ok] = execute("create exchange mynondurableexchange;"),
  [{_, Result}] = execute("select * from exchanges where name=mynondurableexchange and 'durable'=false;"),
  ?assertEqual(1, length(Result)),
  ok.

create_durable_exchange_test() ->
  [ok] = execute("create durable exchange mydurableexchange;"),
  [{_, Result}] = execute("select * from exchanges where name=mydurableexchange and 'durable'=true;"),
  ?assertEqual(1, length(Result)),
  ok.

drop_exchange_test() ->
  [ok, ok] = execute("create exchange myexchangefordropping; drop exchange myexchangefordropping;"),
  [{_, Result}] = execute("select * from exchanges where name=myexchangefordropping;"),
  ?assertEqual(0, length(Result)),
  ok.

create_vhost_test() ->
  [ok] = execute("create vhost '/mytestvhost';"),
  [{_, Result}] = execute("select * from vhosts where name='/mytestvhost';"),
  ?assertEqual(1, length(Result)),
  ok.

drop_vhost_test() ->
  [ok, ok] = execute("create vhost '/mytestvhostfordropping'; drop vhost '/mytestvhostfordropping';"),
  [{_, Result}] = execute("select * from vhosts where name='/mytestvhostfordropping';"),
  ?assertEqual(0, length(Result)),
  ok.

create_user_test() ->
  [ok] = execute("create user anewuser identified by password;"),
  [{_, Result}] = execute("select * from users where name=anewuser;"),
  ?assertEqual(1, length(Result)),
  ok.

drop_user_test() ->
  [ok, ok] = execute("create user anotheruserfordropping identified by secret; drop user anotheruserfordropping;"),
  [{_, Result}] = execute("select * from users where name=anotheruserfordropping;"),
  ?assertEqual(0, length(Result)),
  ok.

select_permission_with_where_clause_not_in_result_test() ->
  [{_, Result}] = execute("select username from permissions where username='guest' and read_perm='.*';"),
  ?assertEqual([[<<"guest">>]], Result),
  ok.

select_queue_with_where_clause_not_in_result_test() ->
  [ok] = execute("create queue mynondurablequeue;"),
  [{_, Result}] = execute("select 'durable' from queues where name='mynondurablequeue' and 'durable'=false;"),
  ?assertEqual([[false]], Result),
  ok.

select_exchange_with_where_clause_not_in_result_test() ->
  [ok] = execute("create exchange mynondurableexchange;"),
  [{_, Result}] = execute("select 'durable' from exchanges where name='mynondurableexchange' and 'durable'=false;"),
  ?assertEqual([[false]], Result),
  ok.

select_binding_with_where_clause_not_in_result_test() ->
  [ok, ok] = execute("create exchange mynondurableexchange; create queue mynondurablequeue;"),
  [ok] = execute("create route from mynondurableexchange to mynondurablequeue;"),
  [{_, Result}] = execute("select queue_name from bindings where exchange_name='mynondurableexchange';"),
  ?assertEqual([["mynondurablequeue"]], Result),
  ok.

send_message_test() ->
  [ok] = execute("create exchange mynondurableexchange;"),
  [ok] = execute("send 'Hello World' to mynondurableexchange;").  

send_message_with_routing_key_test() ->
  [ok] = execute("create exchange mynondurableexchange;"),
  [ok] = execute("send 'Hello World' to mynondurableexchange with routing_key rk;").

execute(Command) ->
  {ok, Result} = bql_server:send_command(<<"guest">>, <<"guest">>, Command),
  Result.

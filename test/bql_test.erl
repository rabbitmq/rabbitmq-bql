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

order_with_single_field_test() ->
    [ok,ok,ok] = execute("create queue myqueue3; create queue myqueue2; create queue myqueue1"),
    [{_, Result}] = execute("select name from queues where name like 'myqueue%' order by name;"),
    ?assertEqual([["myqueue1"], ["myqueue2"], ["myqueue3"]], Result),
    ok.

order_with_multiple_field_test() ->
    [ok,ok,ok] = execute("create topic exchange myx3; create topic exchange myx2; create headers exchange myx1"),
    [{_, Result}] = execute("select name from exchanges where name like 'myx%' order by type desc, name;"),
    ?assertEqual([["myx2"], ["myx3"], ["myx1"]], Result),
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

select_connections_test() ->
    [{_, Result}] = execute("select * from connections"),
    ?assert(length(Result) > 0).

post_message_test() ->
    [ok] = execute("create exchange mynondurableexchange;"),
    [ok] = execute("post 'Hello World' to mynondurableexchange;").  

post_message_with_routing_key_test() ->
    [ok] = execute("create exchange mynondurableexchange;"),
    [ok] = execute("post 'Hello World' to mynondurableexchange with routing_key rk;").

purge_queue_test() ->
    [ok] = execute("create queue mypurgingqueue;"),
    [{ok, 0}] = execute("purge queue mypurgingqueue;"),
    [ok] = execute("post 'Some Message' to '' with routing_key 'mypurgingqueue';"),
    [{ok, 1}] = execute("purge queue mypurgingqueue;").

retrieve_message_test() ->
    [ok, ok] = execute("create exchange mydeliveryexchange; create queue mydeliveryqueue;"),
    [{ok, 0}] = execute("purge queue mydeliveryqueue;"),
    [ok] = execute("create route from mydeliveryexchange to mydeliveryqueue;"),
    [ok] = execute("post 'Some Message' to mydeliveryexchange;"),
    [Response1] = execute("get from mydeliveryqueue;"),
    ?assertEqual(<<"Some Message">>, Response1),
    [{ok, 0}] = execute("purge queue mydeliveryqueue;"),
    [Response2] = execute("get from mydeliveryqueue;"),
    ?assertEqual(empty, Response2),
    [Response3] = execute("get from bogusqueue;"),
    ?assertEqual(unknown_queue, Response3).
    
select_exchange_with_raw_test() ->
    Result = execute_raw([{select, "exchanges", [name], {{eq, name, "amq.topic"}, none}}]),
    ?assertEqual([{[name], [["amq.topic"]]}], Result).

execute(Command) ->
    {ok, Result} = bql_server:send_command(<<"guest">>, <<"guest">>, <<"text/bql">>, Command),
    Result.
    
execute_raw(Terms) ->
    Formatted = lists:flatten(io_lib:format("~p", [Terms])),
    {ok, Result} = bql_server:send_command(<<"guest">>, <<"guest">>, <<"application/bql-terms">>,
                                           Formatted),
    Result.

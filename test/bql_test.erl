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

execute(Command) ->
  {ok, Result} = bql_server:send_command(<<"guest">>, <<"guest">>, Command),
  Result.

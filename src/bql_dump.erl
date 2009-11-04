%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ BQL Plugin.
%%
%%   The Initial Developers of the Original Code are LShift Ltd.
%%
%%   Copyright (C) 2009 LShift Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%
-module(bql_dump).

% Client application for dumping the entire state of the Broker to BQL

-export([start/0, stop/0]).

-define(ReservedExchanges, ["amq.match", "amq.headers", "amq.topic", "amq.direct", "amq.fanout", 
                            "", "amq.rabbitmq.log", "bql.query"]).
-define(ReservedQueues, ["bql.query"]).

start() ->
    Exchanges = execute_block("select * from exchanges order by name;",
                              fun(Ex) ->
                                {value, {_, ExchangeType}} = lists:keysearch(type, 1, Ex),
                                Durable = durable_str(Ex),
                                {value, {_, Name}} = lists:keysearch(name, 1, Ex),
                               
                                case lists:member(Name, ?ReservedExchanges) of
                                  true -> "";
                                  false -> io_lib:format("create ~s~p exchange '~s';", [Durable, ExchangeType, Name])
                                end
                              end),
    Queues = execute_block("select * from queues order by name;",
                           fun(Q) ->
                             Durable = durable_str(Q),
                             {value, {_, Name}} = lists:keysearch(name, 1, Q),
                             
                             case lists:member(Name, ?ReservedQueues) of
                               true  -> "";
                               false -> io_lib:format("create ~squeue '~s';", [Durable, Name])
                             end
                           end),
    Bindings = execute_block("select * from bindings order by exchange_name, queue_name, 'routing_key';",
                             fun(B) ->
                               {value, {_, X}} = lists:keysearch(exchange_name, 1, B),
                               {value, {_, Q}} = lists:keysearch(queue_name, 1, B),
                               {value, {_, RK}} = lists:keysearch(routing_key, 1, B),
                              
                               case {X,binary_to_list(RK)} == {"",Q} of
                                 true ->
                                   %% Auto-route from the default exchange to a queue. Skip.
                                   "";
                                 false -> 
                                   io_lib:format("create route from '~s' to '~s' when routing_key is '~s';",
                                                 [X, Q, RK])
                               end
                             end),

    io:format("~s~n", [string:join([Exchanges, Queues, Bindings], "\n")]),
    init:stop().

stop() ->
    ok.

execute_block(Contents, Formatter) ->
    case rpc:call(localnode(rabbit), bql_server, send_command, [<<"guest">>,
<<"guest">>, <<"text/bql">>, Contents]) of	
        {ok, Result}    -> format(Result, Formatter);
        {error, Reason} -> io:format("BQL execution failed:~n  ~s~n", [Reason])
    end.

durable_str(Row) ->
    case lists:keysearch(durable, 1, Row) of
        {value, {_, true}} -> "durable ";
        _                  -> ""
    end.


format([{Headers, Rows}], Formatter) ->
    Zipped = [lists:zip(Headers, Row) || Row <- Rows],
    Formatted = [Formatter(Row) || Row <- Zipped],
    lists:flatten(string:join([F || F <- Formatted, not(length(F) == 0)], "\n")).

localnode(Name) ->
    %% Imported from rabbit_misc to remove the dependency on the Rabbit server!
    %% This is horrible, but there doesn't seem to be a way to split a
    %% nodename into its constituent parts.
    list_to_atom(lists:append(atom_to_list(Name),
                              lists:dropwhile(fun (E) -> E =/= $@ end,
                                              atom_to_list(node())))).

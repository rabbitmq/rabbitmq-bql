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
-module(amq_interface_test).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

submit_create_command_test() ->
    Response = send_request("create exchange myexchange;"),
    ?assertEqual(<<"{\"success\":true,\"messages\":[\"ok\"]}">>, Response).

submit_query_test() ->
    Response = send_request("select * from vhosts where name='/';"),
    ?assertEqual(<<"{\"success\":true,\"messages\":[[{\"name\":\"/\"}]]}">>, Response).

submit_badly_formatted_query_test() ->
    Response = send_request("create invalidexchange myexchange;"),
    ?assertEqual(<<"{\"success\":false,\"message\":\"syntax error before: \\\"invalidexchange\\\" on line 1\"}">>, Response).

submit_query_against_non_existant_object_test() ->
    Response = send_request("select * from something;"),
    ?assertEqual(<<"{\"success\":true,\"messages\":[\"Unknown entity something specified to query\"]}">>, Response).

send_request(Content) ->
    {ok, Connection} = amqp_connection:start(direct, #amqp_params{}),
    Client = bql_amqp_rpc_client:start(Connection, <<"bql.query">>),
    Res = bql_amqp_rpc_client:call(Client, <<"application/json">>,
                                   list_to_binary("{\"query\":\"" ++ Content ++ "\"}"), 500),
    amqp_connection:close(Connection),
    Res.


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
-module(bql_client_test).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("eunit/include/eunit.hrl").

submit_create_command_test() ->
    Response = send_request("create exchange myexchange;"),
    ?assertEqual({ok, [ok]}, Response).

submit_query_test() ->
    Response = send_request("select * from vhosts where name='/';"),
    ?assertEqual({ok, [{[name], [["/"]]}]}, Response).

submit_badly_formatted_query_test() ->
    Response = send_request("create invalidexchange myexchange;"),
    ?assertEqual({error, "syntax error before: \"invalidexchange\" on line 1"}, Response).

submit_query_against_non_existant_object_test() ->
    Response = send_request("select * from something;"),
    ?assertEqual({ok, ["Unknown entity something specified to query"]}, Response).

send_request(Content) ->
    Client = bql_client:connect("localhost", ?PROTOCOL_PORT, <<"guest">>, <<"guest">>, <<"/">>),
    Res = bql_client:execute(Client, Content),
    bql_client:close(Client),
    Res.

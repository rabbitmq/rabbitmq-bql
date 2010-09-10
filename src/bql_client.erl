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
-module(bql_client).

-export([connect/0, connect/5, close/1, execute/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

% Record defining the context in which BQL commands are executed
-record(client_ctx, {username, password, vhost, connection, rpc_client}).

%% Creates a connection to the Rabbit server that can subsequently be used
%% to issue BQL requests. Uses init arguments to determine connection
%% parameters.
connect() ->
  Username = list_to_binary(bql_utils:argument_or_default(username, "guest")),
  Password = list_to_binary(bql_utils:argument_or_default(password, "guest")),
  VHost = list_to_binary(bql_utils:argument_or_default(vhost, "/")),
  Host = bql_utils:argument_or_default(host, "localhost"),
  Port = list_to_integer(bql_utils:argument_or_default(port, integer_to_list(?PROTOCOL_PORT))),
  connect(Host, Port, Username, Password, VHost).

%% Creates a connection to the Rabbit server that can subsequently be used
%% to issue BQL requests.
connect(Host, Port, Username, Password, VHost) ->
	% Open a conenction and wire a RPC client to it
	{ok, Connection} = amqp_connection:start(network, #amqp_params{
        username     = Username,
        password     = Password,
        virtual_host = <<"/">>,  %% bql.query is in the default vhost
        host         = Host,
        port         = Port}),
    Client = bql_amqp_rpc_client:start(Connection, <<"bql.query">>),
    #client_ctx{username = Username, password = Password, vhost = VHost,
	            connection = Connection, rpc_client = Client}.
				
%% Disconnects the BQL client and frees up any resources associated with it
close(#client_ctx { connection = Connection, rpc_client = Client }) ->
  bql_amqp_rpc_client:stop(Client),
  amqp_connection:close(Connection).
				
%% Executes the given BQL request on the connected server
execute(#client_ctx { username = User, password = Password, vhost = VHost, rpc_client = Client }, Contents) ->
  Request = [{user, User}, {password, Password}, {vhost, VHost}, {query_text, Contents}],
	Res = bql_amqp_rpc_client:call(Client, <<"application/bert">>, term_to_binary(Request), 500),
	binary_to_term(Res).

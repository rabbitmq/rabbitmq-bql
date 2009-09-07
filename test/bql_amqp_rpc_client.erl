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
-module(bql_amqp_rpc_client).

-include_lib("amqp_client/include/amqp_client.hrl").

-behaviour(gen_server).

-export([start/2, stop/1]).
-export([call/4, call/5]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).

%---------------------------------------------------------------------------
% API
%---------------------------------------------------------------------------

start(Connection, RoutingKey) ->
    {ok, Pid} = gen_server:start(?MODULE, [Connection, RoutingKey], []),
    Pid.

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

call(RpcClientPid, Exchange, ContentType, Payload) ->
    call(RpcClientPid, Exchange, ContentType, Payload, infinity).
call(RpcClientPid, Exchange, ContentType, Payload, Timeout) ->
    gen_server:call(RpcClientPid, {call, Exchange, ContentType, Payload}, Timeout).

%---------------------------------------------------------------------------
% Plumbing
%---------------------------------------------------------------------------

% Sets up a reply queue for this client to listen on
setup_reply_queue(State = #rpc_client_state{channel = Channel}) ->
    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, #'queue.declare'{}),
    State#rpc_client_state{reply_queue = Q}.

% Registers this RPC client instance as a consumer to handle rpc responses
setup_consumer(#rpc_client_state{channel = Channel,
                                 reply_queue = Q}) ->
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Q}, self()).

% Publishes to the broker, stores the From address against
% the correlation id and increments the correlationid for
% the next request
publish(Exchange, ContentType, Payload, From,
        State = #rpc_client_state{channel = Channel,
                                  reply_queue = Q,
                                  routing_key = RoutingKey,
                                  correlation_id = CorrelationId,
                                  continuations = Continuations}) ->
    Props = #'P_basic'{correlation_id = <<CorrelationId:64>>,
                       content_type = ContentType,
                       reply_to = Q},
    amqp_channel:call(Channel, #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
                      #amqp_msg{payload = Payload, props = Props}),
    State#rpc_client_state{correlation_id = CorrelationId + 1,
                           continuations
                           = dict:store(CorrelationId, From, Continuations)}.

%---------------------------------------------------------------------------
% gen_server callbacks
%---------------------------------------------------------------------------

% Sets up a reply queue and consumer within an existing channel
init([Connection, RoutingKey]) ->
    Channel = amqp_connection:open_channel(Connection),
    InitialState = #rpc_client_state{channel = Channel,
                                     exchange = <<>>,
                                     routing_key = RoutingKey},
    State = setup_reply_queue(InitialState),
    setup_consumer(State),
    {ok, State}.

% Closes the channel this gen_server instance started
terminate(_Reason, #rpc_client_state{channel = Channel}) ->
    amqp_channel:close(Channel),
    ok.

% Handle the application initiated stop by just stopping this gen server
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({call, Exchange, ContentType, Payload}, From, State) ->
    NewState = publish(Exchange, ContentType, Payload, From, State),
    {noreply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};

handle_info({#'basic.deliver'{},
            #amqp_msg{props = #'P_basic'{correlation_id = <<Id:64>>},
                      payload = Payload }},
            State = #rpc_client_state{continuations = Conts}) ->
    From = dict:fetch(Id, Conts),
    gen_server:reply(From, binary_to_list(Payload)),
    {noreply, State#rpc_client_state{continuations = dict:erase(Id, Conts) }}.

code_change(_OldVsn, State, _Extra) ->
    State.


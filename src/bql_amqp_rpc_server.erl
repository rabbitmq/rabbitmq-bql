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
%%   The Original Code is the RabbitMQ BQL Module.
%%
%%   The Initial Developers of the Original Code are LShift Ltd.,
%%   Cohesive Financial Technologies LLC., and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd., Cohesive Financial
%%   Technologies LLC., and Rabbit Technologies Ltd. are Copyright (C)
%%   2009 LShift Ltd., Cohesive Financial Technologies LLC., and Rabbit
%%   Technologies Ltd.;
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ___________________________
%%
-module(bql_amqp_rpc_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("rabbit.hrl").
-include("amqp_client.hrl").
-include("rabbit_framing.hrl").

-record(state, { channel }).

-define(ExchangeName, <<"bql.query">>).
-define(QueueName, <<"bql.query">>).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    Connection = lib_amqp:start_connection(),
    Ch = amqp_connection:open_channel(Connection),
    link(Ch),
    
    _X = amqp_channel:call(Ch, #'exchange.declare'{exchange = ?ExchangeName, durable = true}),
    Q = lib_amqp:declare_queue(Ch, #'queue.declare'{queue = ?QueueName, durable = true}),
    _ConsumerTag = lib_amqp:subscribe(Ch, Q, self()),
    #'queue.bind_ok'{} = lib_amqp:bind_queue(Ch, ?ExchangeName, Q, <<>>),

    {ok, #state { channel = Ch } }.

handle_call(_,_,State) -> {reply,unhandled_call,State}.
handle_cast(_,State) -> {reply,unhandled_cast,State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver' { 'delivery_tag' = DeliveryTag },
             #amqp_msg{props = Props, payload = Payload }},
            State = #state { channel = Ch }) ->
    #'P_basic'{correlation_id = CorrelationId, reply_to = Q} = Props,
    try
      ResponseObj = case rfc4627:decode(Payload) of
        {ok, RequestObj, _Rest} ->
          case rfc4627:get_field(RequestObj, "query") of 
            {ok, Query} ->
              case bql_server:send_command(<<"guest">>, <<"guest">>, <<"text/bql">>, binary_to_list(Query)) of
                {ok, Result} ->
                  {obj, [{"success", true}, {"messages", format_result(Result)}]};
                {error, Reason} ->
                  {obj, [{"success", false}, {"message", list_to_binary(Reason)}]}
              end;
            _ ->
              {obj, [{"success", false}, {"message", <<"Invalid request - no query attribute">>}]}
          end;
        {error, _Reason} ->
          {obj, [{"success", false}, {"message", <<"Invalid JSON in Query">>}]}
      end,

      Properties = #'P_basic'{correlation_id = CorrelationId},
      lib_amqp:publish(Ch, <<>>, Q, rfc4627:encode(ResponseObj), Properties)
    catch
      Tag:Error -> io:fwrite("Caught error: ~p,~p~n", [Tag, Error])
    end,
    lib_amqp:ack(Ch, DeliveryTag),
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, channel_shutdown, State};
handle_info(_Info, State) ->
    {reply, unhandled_info, State}.

terminate(_Reason, #state { channel = Ch }) ->
    case is_process_alive(Ch) of
        true -> lib_amqp:close_channel(Ch);
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_result(Result) ->
    [format_result_entry(E) || E <- Result].

format_result_entry(ok) ->
    <<"ok">>;
format_result_entry({Headers, Rows}) ->
    [{obj, [{atom_to_list(Header), list_to_binary(bql_utils:convert_to_string(Cell))} || 
                {Header, Cell} <- lists:zip(Headers, Row)]} || Row <- Rows];
format_result_entry(Msg) when is_list(Msg) ->
    list_to_binary(Msg).

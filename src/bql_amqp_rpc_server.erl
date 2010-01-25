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
-module(bql_amqp_rpc_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, { channel }).

-define(ExchangeName, <<"bql.query">>).
-define(QueueName, <<"bql.query">>).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    Connection = amqp_connection:start_direct(#amqp_params{}),
    Ch = amqp_connection:open_channel(Connection),
    link(Ch),

    #'queue.declare_ok'{} = amqp_channel:call(Ch, #'queue.declare'{queue = ?QueueName, durable = true}),
    _ConsumerTag = amqp_channel:call(Ch, #'basic.consume'{queue = ?QueueName}),
    {ok, #state { channel = Ch } }.

handle_call(_,_,State) -> {reply,unhandled_call,State}.
handle_cast(_,State) -> {reply,unhandled_cast,State}.

handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver' { 'delivery_tag' = DeliveryTag },
             #amqp_msg{props = Props, payload = Payload }},
            State = #state { channel = Ch }) ->
    #'P_basic'{correlation_id = CorrelationId, reply_to = Q, content_type = ContentType} = Props,
    try
      Result = case decode_request(ContentType, Payload) of
        {ok, User, Password, VHost, Query} ->
          bql_server:send_command(User, Password, VHost, <<"text/bql">>, Query);
        {error, Reason} -> {error, Reason}
      end,
      Response = encode_result(ContentType, Result),
      
      Properties = #'P_basic'{correlation_id = CorrelationId},
      amqp_channel:call(Ch, #'basic.publish'{exchange = <<>>, routing_key = Q}, 
                        #amqp_msg{payload = Response, props = Properties})
    catch
      Tag:Error -> io:fwrite("Caught error: ~p,~p,~p~n", [Tag, Error,
                              erlang:get_stacktrace()])
    end,
    ok = amqp_channel:cast(Ch, #'basic.ack'{delivery_tag = DeliveryTag}),
    {noreply, State};
handle_info(shutdown, State) ->
    {stop, channel_shutdown, State};
handle_info(_Info, State) ->
    {reply, unhandled_info, State}.

terminate(_Reason, #state { channel = Ch }) ->
    case is_process_alive(Ch) of
        true -> amqp_channel:close(Ch);
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

decode_request(<<"application/json">>, Payload) ->
	case rfc4627:decode(Payload) of
    {ok, RequestObj, _Rest} ->
      case rfc4627:get_field(RequestObj, "query") of 
        {ok, Query} ->
			    {ok, 
			      get_field_or_default(RequestObj, "user", <<"guest">>), 
			      get_field_or_default(RequestObj, "password", <<"guest">>), 
			      get_field_or_default(RequestObj, "vhost", <<"/">>), 
			      binary_to_list(Query)};
        _ ->
          {error, <<"Invalid request - no query attribute">>}
      end;
    {error, _Reason} ->
      {error, <<"Invalid JSON in Query">>}
  end;
decode_request(<<"application/bert">>, Payload) ->
  Request = binary_to_term(Payload),
  {ok, 
    proplists:get_value(user, Request, <<"guest">>),
    proplists:get_value(password, Request, <<"guest">>),
    proplists:get_value(vhost, Request, <<"/">>),
    proplists:get_value(query_text, Request, "")}.
  
encode_result(<<"application/json">>, Result) ->
  ResponseObj = case Result of
    {ok, ResultEls} ->
      {obj, [{"success", true}, {"messages", format_result(ResultEls)}]};
    {error, Reason} ->
      {obj, [{"success", false}, {"message", list_to_binary(Reason)}]}
  end,
  
  list_to_binary(rfc4627:encode(ResponseObj));
encode_result(<<"application/bert">>, Result) ->
  term_to_binary(Result).

get_field_or_default(Obj, Name, Default) ->
  case rfc4627:get_field(Obj, Name) of 
    {ok, Val} -> Val;
    _ -> Default
  end.

format_result(Result) ->
    [format_result_entry(E) || E <- Result].

format_result_entry(ok) ->
    <<"ok">>;
format_result_entry({Headers, Rows}) ->
    [{obj, [{atom_to_list(Header), list_to_binary(bql_utils:convert_to_string(Cell))} || 
                {Header, Cell} <- lists:zip(Headers, Row)]} || Row <- Rows];
format_result_entry(Msg) when is_list(Msg) ->
    list_to_binary(Msg).

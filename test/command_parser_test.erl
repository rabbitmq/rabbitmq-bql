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
%%   The Original Code is the RabbitMQ Erlang Client.
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
-module(command_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define(debugCommands(C), ?debugFmt("Got commands: ~p~n", [C])). 

create_nondurable_queue_test() ->
    {ok, Commands} = commands:parse("create queue blah;"),
    ?assert([{create_queue,"blah",false}] =:= Commands).

create_nondurable_queue_with_space_in_name_test() ->
    {ok, Commands} = commands:parse("create queue 'bl ah';"),
    ?assert([{create_queue,"bl ah",false}] =:= Commands).

create_nondurable_queue_with_exotic_name_test() ->
    {ok, Commands} = commands:parse("create queue b.a-t_b;"),
    ?assert([{create_queue,"b.a-t_b",false}] =:= Commands).

create_durable_queue_test() ->
    {ok, Commands} = commands:parse("create durable queue 'blah';"),
    ?assert([{create_queue,"blah",true}] =:= Commands).

create_multiple_queues_test() ->
    {ok, Commands} = commands:parse("create durable queue 'blah'; create queue 'blah2';"),
    ?assert([{create_queue,"blah",true}, {create_queue,"blah2", false}] =:= Commands).

drop_queue_test() ->
    {ok, Commands} = commands:parse("drop queue 'myqueue';"),
    ?assert([{drop_queue,"myqueue"}] =:= Commands).

create_default_exchange_test() ->
    {ok, Commands} = commands:parse("create exchange 'myex';"),
    ?assert([{create_exchange,"myex",direct,false}] =:= Commands).

create_direct_exchange_test() ->
    {ok, Commands} = commands:parse("create direct exchange 'myex';"),
    ?assert([{create_exchange,"myex",direct,false}] =:= Commands).

create_headers_exchange_test() ->
    {ok, Commands} = commands:parse("create headers exchange 'myex';"),
    ?assert([{create_exchange,"myex",headers,false}] =:= Commands).

create_fanout_exchange_test() ->
    {ok, Commands} = commands:parse("create fanout exchange 'myex';"),
    ?assert([{create_exchange,"myex",fanout,false}] =:= Commands).

create_durable_default_exchange_test() ->
    {ok, Commands} = commands:parse("create durable exchange 'myex';"),
    ?assert([{create_exchange,"myex",direct,true}] =:= Commands).

create_durable_direct_exchange_test() ->
    {ok, Commands} = commands:parse("create durable direct exchange 'myex';"),
    ?assert([{create_exchange,"myex",direct,true}] =:= Commands).

create_durable_headers_exchange_test() ->
    {ok, Commands} = commands:parse("create durable headers exchange 'myex';"),
    ?assert([{create_exchange,"myex",headers,true}] =:= Commands).

create_durable_fanout_exchange_test() ->
    {ok, Commands} = commands:parse("create durable fanout exchange 'myex';"),
    ?assert([{create_exchange,"myex",fanout,true}] =:= Commands).

drop_exchange_test() ->
    {ok, Commands} = commands:parse("drop exchange 'myex';"),
    ?assert([{drop_exchange,"myex"}] =:= Commands).

create_binding_with_no_routing_key_test() ->
    {ok, Commands} = commands:parse("create route from 'myex' to 'myqueue';"),
    ?assert([{create_binding,{"myex","myqueue",""}}] =:= Commands).

create_binding_with_routing_key_test() ->
    {ok, Commands} = commands:parse("create route from 'myex' to 'myqueue' when routing_key is 'Hello';"),
    ?assert([{create_binding,{"myex","myqueue","Hello"}}] =:= Commands).

drop_binding_with_no_routing_key_test() ->
    {ok, Commands} = commands:parse("drop route from 'myex' to 'myqueue';"),
    ?assert([{drop_binding,{"myex","myqueue",""}}] =:= Commands).

drop_binding_with_routing_key_test() ->
    {ok, Commands} = commands:parse("drop route from 'myex' to 'myqueue' when routing_key is 'Hello';"),
    ?assert([{drop_binding,{"myex","myqueue","Hello"}}] =:= Commands).

%create_queue_with_bad_character_test() ->
%    {error, Reason} = commands:parse("create queue 'queue';\n\n\ncreate queue 'queue%';"),
%    ?assert("Illegal token \"'queue%\" on line 4" =:= Reason).

create_queue_with_missing_end_quote_test() ->
    {error, Reason} = commands:parse("create queue 'queue';\n\n\ncreate queue 'queue;"),
    ?assert("Illegal token \"'queue;\" on line 4" =:= Reason).

create_exchange_of_invalid_type_test() ->
    {error, Reason} = commands:parse("create queue 'queue';\n\n\ncreate interesting exchange 'queue';"),
    ?assert("syntax error before: \"interesting\" on line 4" =:= Reason).

select_all_exchange_details_test() ->
    {ok, Commands} = commands:parse("select * from exchanges;"),
    ?assert([{select, "exchanges", all, {none, none}}] =:= Commands).

select_exchange_names_test() ->
    {ok, Commands} = commands:parse("select name from exchanges;"),
    ?assert([{select, "exchanges", [name], {none, none}}] =:= Commands).

select_exchange_names_and_types_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges;"),
    ?assert([{select, "exchanges", [name, type], {none, none}}] =:= Commands).

select_exchange_name_types_with_filter_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges where name='amq.topic';"),
    ?assert([{select, "exchanges", [name, type], {{eq, name, "amq.topic"}, none}}] =:= Commands).

select_exchange_name_types_with_anded_filter_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges where name='amq.topic' and 'durable'!=true;"),
    ?assert([{select, "exchanges", [name, type], {{and_sym, {eq, name, "amq.topic"}, {neq, durable, "true"}}, none}}] =:= Commands).

select_exchange_name_types_with_ored_filter_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges where name='amq.topic' or 'durable'!=true;"),
    ?assert([{select, "exchanges", [name, type], {{or_sym, {eq, name, "amq.topic"}, {neq, durable, "true"}}, none}}] =:= Commands).

select_exchange_name_order_by_name_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges order by name;"),
    ?assert([{select, "exchanges", [name, type], {none, {order_by, name, ascending}}}] =:= Commands).

select_exchange_name_order_by_name_explicit_descending_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges order by name desc;"),
    ?assert([{select, "exchanges", [name, type], {none, {order_by, name, descending}}}] =:= Commands).

select_exchange_name_order_by_name_explicit_ascending_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges order by name asc;"),
    ?assert([{select, "exchanges", [name, type], {none, {order_by, name, ascending}}}] =:= Commands).

select_exchange_name_order_by_name_with_constraints_test() ->
    {ok, Commands} = commands:parse("select name,type from exchanges where name='amq.topic' or 'durable'!=true order by name;"),
    ?assert([{select, "exchanges", [name, type], {{or_sym, {eq, name, "amq.topic"}, {neq, durable, "true"}}, {order_by, name, ascending}}}] =:= Commands).

create_user_test() ->
    {ok, Commands} = commands:parse("create user user1 identified by mypassword;"),
    ?assert([{create_user, "user1", "mypassword"}] =:= Commands).

drop_user_test() ->
    {ok, Commands} = commands:parse("drop user user1;"),
    ?assert([{drop_user, "user1"}] =:= Commands).

select_binding_with_routing_key_like_test() ->
    {ok, Commands} = commands:parse("select * from bindings where 'routing_key' like 'amq.%';"),
    ?assertEqual([{select, "bindings", all, {{like, routing_key, "amq.%"}, none}}], Commands).

create_vhost_test() ->
    {ok, Commands} = commands:parse("create vhost '/myhost';"),
    ?assert([{create_vhost, "/myhost"}] =:= Commands).

drop_vhost_test() ->
    {ok, Commands} = commands:parse("drop vhost '/myhost';"),
    ?assert([{drop_vhost, "/myhost"}] =:= Commands).

grant_permission_test() ->
    {ok, Commands} = commands:parse("grant all on '.*' to 'user';"),
    ?assertEqual([{grant, all, ".*", "user"}], Commands).

revoke_permission_test() ->
    {ok, Commands} = commands:parse("revoke configure from 'user';"),
    ?assert([{revoke, configure, "user"}] =:= Commands).

purge_queue_test() ->
    {ok, Commands} = commands:parse("purge queue myqueue;"),
    ?assert([{purge_queue, "myqueue"}] =:= Commands).

send_message_test() ->
    {ok, Commands} = commands:parse("send 'Hello World' to myexchange;"),
    ?assert([{send_message, "myexchange", "", "Hello World"}] =:= Commands).

send_message_with_routing_key_test() ->
    {ok, Commands} = commands:parse("send 'Hello World' to myexchange with routing_key rk;"),
    ?assert([{send_message, "myexchange", "rk", "Hello World"}] =:= Commands).

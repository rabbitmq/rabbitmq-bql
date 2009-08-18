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
Nonterminals 
statements expression route_desc field_desc field_list modifiers where_clause orderby_clause predicate predicates
orderby_predicates orderby_predicate.

Terminals
create drop durable queue exchange exchange_type route from to routing_key is when_sym string select wildcard
comma where comparator union order by asc desc user identified vhost grant revoke on purge post with get semi
drain.

Rootsymbol statements.

statements -> expression                 : ['$1'].
statements -> expression semi            : ['$1'].
statements -> expression semi statements : ['$1'] ++ '$3'.

expression -> create vhost string                             : {create_vhost, unwrap('$3')}.
expression -> drop vhost string                               : {drop_vhost, unwrap('$3')}.
expression -> create queue string                             : {create_queue, unwrap('$3'), false, ""}.
expression -> create durable queue string                     : {create_queue, unwrap('$4'), true, ""}.
expression -> drop queue string                               : {drop_queue, unwrap('$3')}.
expression -> create exchange string                          : {create_exchange, unwrap('$3'), direct, false, ""}.
expression -> create durable exchange string                  : {create_exchange, unwrap('$4'), direct, true, ""}.
expression -> create exchange_type exchange string            : {create_exchange, unwrap('$4'), unwrap('$2'), false, ""}.
expression -> create durable exchange_type exchange string    : {create_exchange, unwrap('$5'), unwrap('$3'), true, ""}.
expression -> drop exchange string                            : {drop_exchange, unwrap('$3')}.
expression -> create route_desc                               : {create_binding, '$2'}.
expression -> drop route_desc                                 : {drop_binding, '$2'}.
expression -> create user string identified by string         : {create_user, unwrap('$3'), unwrap('$6')}.
expression -> drop user string                                : {drop_user, unwrap('$3')}.
expression -> select field_desc from string                   : {select, unwrap('$4'), '$2', {none, none}}.
expression -> select field_desc from string modifiers         : {select, unwrap('$4'), '$2', '$5'}.
expression -> grant string on string to string                : {grant, list_to_atom(unwrap('$2')), unwrap('$4'), unwrap('$6')}.
expression -> revoke string from string                       : {revoke, list_to_atom(unwrap('$2')), unwrap('$4')}.
expression -> purge queue string                              : {purge_queue, unwrap('$3')}.
expression -> post string to string                           : {post_message, unwrap('$4'), "", unwrap('$2')}.
expression -> post string to string with routing_key string   : {post_message, unwrap('$4'), unwrap('$7'), unwrap('$2')}.
expression -> get from string                                 : {retrieve_message, unwrap('$3')}.
expression -> drain string                                    : {drain_queue, unwrap('$2')}.

route_desc -> route from string to string                                 : {unwrap('$3'), unwrap('$5'), ""}.
route_desc -> route from string to string when_sym routing_key is string  : {unwrap('$3'), unwrap('$5'), unwrap('$9')}.

field_desc -> wildcard                                        : all.
field_desc -> field_list                                      : '$1'.
field_list -> string                                          : [list_to_atom(unwrap('$1'))].
field_list -> string comma field_list                         : [list_to_atom(unwrap('$1'))] ++ '$3'.

modifiers  -> where_clause                                    : {'$1', none}.
modifiers  -> where_clause orderby_clause                     : {'$1', '$2'}.
modifiers  -> orderby_clause                                  : {none, '$1'}.

where_clause -> where predicates                              : '$2'.

predicates -> predicate                                       : '$1'.
predicates -> predicate union predicate                       : {unwrap('$2'), '$1', '$3'}.

predicate -> string comparator string                         : {comp_to_atom(unwrap('$2')), list_to_atom(unwrap('$1')), unwrap('$3')}.

orderby_clause -> order by orderby_predicates                    : {order_by, '$3'}.

orderby_predicates -> orderby_predicate                          : ['$1'].
orderby_predicates -> orderby_predicate comma orderby_predicates : ['$1'] ++ '$3'.

orderby_predicate -> string                                   : {list_to_atom(unwrap('$1')), ascending}.
orderby_predicate -> string asc                               : {list_to_atom(unwrap('$1')), ascending}.
orderby_predicate -> string desc                              : {list_to_atom(unwrap('$1')), descending}.

Erlang code.

unwrap({_,_,V}) -> V.

comp_to_atom('=')  -> eq;
comp_to_atom('!=') -> neq;
comp_to_atom('<>') -> neq;
comp_to_atom('<')  -> lt;
comp_to_atom('<=') -> lteq;
comp_to_atom('>')  -> gt;
comp_to_atom('>=') -> gteq;
comp_to_atom(Atom) when is_atom(Atom) -> Atom.

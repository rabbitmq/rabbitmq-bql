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
-module(bql_applicator).

-export([apply_commands/1]).

-include("amqp_client.hrl").
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-define(RPC_TIMEOUT, 30000).

-record(state, {ch, node}).

apply_commands(Commands) ->
  % Create an AMQP channel so we can actually perform operations
  Connection = lib_amqp:start_connection("localhost"),
  ControlCh = amqp_connection:open_channel(Connection),

  % Create a connection to the Rabbit node too
  Node = rabbit_misc:localnode(rabbit),

  try
      {ok, [catch apply_command(#state {ch = ControlCh, node = Node}, Command) || Command <- Commands]}
  after
      lib_amqp:close_connection(Connection)
  end.

% Queue Management
apply_command(#state {ch = ControlCh}, {create_queue, Name, Durable}) ->
  lib_amqp:declare_queue(ControlCh, #'queue.declare'{queue = list_to_binary(Name), durable = Durable}),
  ok;
apply_command(#state {ch = ControlCh}, {drop_queue, Name}) ->
  lib_amqp:delete_queue(ControlCh, list_to_binary(Name)),
  ok;
apply_command(#state {ch = ControlCh}, {purge_queue, Name}) ->
  amqp_channel:call(ControlCh, #'queue.purge'{queue = list_to_binary(Name)}),
  ok;

% Exchange Management
apply_command(#state {ch = ControlCh}, {create_exchange, Name, Type, Durable}) ->
  amqp_channel:call(ControlCh, #'exchange.declare'{exchange = list_to_binary(Name),
                                                   type = list_to_binary(atom_to_list(Type)),
                                                   durable = Durable}),
  ok;
apply_command(#state {ch = ControlCh}, {drop_exchange, Name}) ->
  lib_amqp:delete_exchange(ControlCh, list_to_binary(Name)),
  ok;

% User Management
apply_command(#state {node = Node}, {create_user, Name, Password}) ->
  rpc_call(Node, rabbit_access_control, add_user, [list_to_binary(Name), list_to_binary(Password)]),
  ok;
apply_command(#state {node = Node}, {drop_user, Name}) ->
  rpc_call(Node, rabbit_access_control, delete_user, [list_to_binary(Name)]),
  ok;

% VHost Management
apply_command(#state {node = Node}, {create_vhost, Name}) ->
  rpc_call(Node, rabbit_access_control, add_vhost, [list_to_binary(Name)]),
  ok;
apply_command(#state {node = Node}, {drop_vhost, Name}) ->
  rpc_call(Node, rabbit_access_control, delete_vhost, [list_to_binary(Name)]),
  ok;

% Binding Management
apply_command(#state {ch = ControlCh}, {create_binding, {X, Q, RoutingKey}}) ->
  lib_amqp:bind_queue(ControlCh, list_to_binary(X), list_to_binary(Q), list_to_binary(RoutingKey)),
  ok;
apply_command(#state {ch = ControlCh}, {drop_binding, {X, Q, RoutingKey}}) ->
  lib_amqp:unbind_queue(ControlCh, list_to_binary(X), list_to_binary(Q), list_to_binary(RoutingKey)),
  ok;

% Privilege Management
apply_command(#state {node = Node}, {grant, Privilege, Regex, User}) ->
  PrivilegeList = expand_privilege_list(Privilege),
  apply_privilege_list(Node, list_to_binary(User), PrivilegeList, list_to_binary(Regex));
apply_command(#state {node = Node}, {revoke, Privilege, User}) ->
  PrivilegeList = expand_privilege_list(Privilege),
  apply_privilege_list(Node, list_to_binary(User), PrivilegeList, <<"">>);
  
% Queries
apply_command(#state {node = Node}, {select, "exchanges", Fields, Modifiers}) ->
  AllFieldList = [name, type, durable, auto_delete, arguments],
  FieldList = validate_fields(AllFieldList, Fields),
  Exchanges = rpc_call(Node, rabbit_exchange, info_all, [<<"/">>, AllFieldList]),
  interpret_response(AllFieldList, FieldList, Exchanges, Modifiers);

apply_command(#state {node = Node}, {select, "queues", Fields, Modifiers}) ->
  AllFieldList = [name, durable, auto_delete, arguments, pid, messages_ready,
                  messages_unacknowledged, messages_uncommitted, messages, acks_uncommitted,
                  consumers, transactions, memory],
  FieldList = validate_fields(AllFieldList, Fields),
  Queues = rpc_call(Node, rabbit_amqqueue, info_all, [<<"/">>, AllFieldList]),
  interpret_response(AllFieldList, FieldList, Queues, Modifiers);

apply_command(#state {node = Node}, {select, "bindings", Fields, Modifiers}) ->
  AllFieldList = [exchange_name, queue_name, routing_key, args],
  FieldList = validate_fields(AllFieldList, Fields),
  Bindings = rpc_call(Node, rabbit_exchange, list_bindings, [<<"/">>]),
  interpret_response(AllFieldList, FieldList, Bindings, Modifiers);

apply_command(#state {node = Node}, {select, "users", Fields, Modifiers}) ->
  AllFieldList = [name],
  FieldList = validate_fields(AllFieldList, Fields),
  Response = rpc_call(Node, rabbit_access_control, list_users, []),
  Users = [[binary_to_list(User)] || User <- Response],
  interpret_response(AllFieldList, FieldList, Users, Modifiers);

apply_command(#state {node = Node}, {select, "vhosts", Fields, Modifiers}) ->
  AllFieldList = [name],
  FieldList = validate_fields(AllFieldList, Fields),
  Response = rpc_call(Node, rabbit_access_control, list_vhosts, []),
  VHosts = [[{name, binary_to_list(User)}] || User <- Response],
  interpret_response(AllFieldList, FieldList, VHosts, Modifiers);

apply_command(#state {node = Node}, {select, "permissions", Fields, Modifiers}) ->
  AllFieldList = [username,configure_perm,write_perm,read_perm],
  FieldList = validate_fields(AllFieldList, Fields),
  Permissions = rpc_call(Node, rabbit_access_control, list_vhost_permissions, [<<"/">>]),
  interpret_response(AllFieldList, FieldList, Permissions, Modifiers);

apply_command(#state {node = Node}, {select, "connections", Fields, Modifiers}) ->
  AllFieldList = [pid, address, port, peer_address, peer_port, recv_oct, recv_cnt, send_oct, send_cnt,
                  send_pend, state, channels, user, vhost, timeout, frame_max],
  FieldList = validate_fields(AllFieldList, Fields),
  Connections = rpc_call(Node, rabbit_networking, connection_info_all, [AllFieldList]),
  interpret_response(AllFieldList, FieldList, Connections, Modifiers);

% Sending Messages
apply_command(#state { ch = Ch }, {post_message, Exchange, RoutingKey, Msg}) ->
  Properties = #'P_basic'{ delivery_mode = 1 },
  case lib_amqp:publish(Ch, list_to_binary(Exchange), list_to_binary(RoutingKey), 
                        list_to_binary(Msg), Properties) of
    ok  -> ok;
    Res -> io_lib:format("~p", [Res])
  end;

% Retreving Messages
apply_command(#state { ch = Ch, node = Node }, {retrieve_message, Queue}) ->
  case rpc_call(Node, rabbit_amqqueue, lookup,
                         [{resource, <<"/">>, queue, list_to_binary(Queue)}]) of
    {error, not_found} ->
      unknown_queue;
    {ok,_} ->
      case lib_amqp:get(Ch, list_to_binary(Queue)) of
       'basic.get_empty'            -> empty;
       #amqp_msg{payload = Payload} -> Payload
      end
  end;

apply_command(#state {}, {select, EntityName, _, _}) ->
  lists:flatten("Unknown entity " ++ EntityName ++ " specified to query");

% Catch-all  
apply_command(_State, Unknown) ->
  debug("Unknown command: ~p~n", [Unknown]).

% Debug Control
debug(_Format, _Params) ->
  ok.
  % io:format(Format, Params).

% RPC Commands
rpc_call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args, ?RPC_TIMEOUT).

% Formatting commands
interpret_response(_, _, {bad_argument, Field}, _) ->
  lists:flatten(io_lib:format("Invalid field \"~p\" requested", [Field]));
interpret_response(_, _, [], _) ->
  [];

interpret_response(AvailFieldList, RequestedFieldList, [RHead|_] = Response, Modifiers) when is_tuple(RHead)->
  interpret_response(AvailFieldList, RequestedFieldList, [tuple_to_list(X) || X <- Response], Modifiers);

interpret_response(AvailFieldList, RequestedFieldList, Response, {Constraints, Ordering}) ->
  FormattedResponse = [[format_response(Cell) || Cell <- Detail] || Detail <- Response],
  ConstrainedResponse = apply_constraints(AvailFieldList, FormattedResponse, Constraints),
  OrderedResponse = apply_ordering(AvailFieldList, ConstrainedResponse, Ordering),
  FilteredResponse = filter_cols(AvailFieldList, RequestedFieldList, OrderedResponse),
  {RequestedFieldList, FilteredResponse}.

format_response({_Name, Value}) ->
  format_response(Value);
format_response({resource, _VHost, _Type, Value}) ->
  binary_to_list(Value);
format_response(Value) ->
  Value.

% Constraints
apply_constraints(_FieldList, Rows, none) ->
  Rows;

apply_constraints(FieldList, Rows, {and_sym, Left, Right}) ->
  LeftRows = apply_constraints(FieldList, Rows, Left),
  RightRows = apply_constraints(FieldList, Rows, Right),
  sets:to_list(sets:intersection(sets:from_list(LeftRows), sets:from_list(RightRows)));
apply_constraints(FieldList, Rows, {or_sym, Left, Right}) ->
  LeftRows = apply_constraints(FieldList, Rows, Left),
  RightRows = apply_constraints(FieldList, Rows, Right),
  sets:to_list(sets:union(sets:from_list(LeftRows), sets:from_list(RightRows)));

apply_constraints(FieldList, Rows, {Constraint, Field, Value}) ->
  FieldPositions = lists:zip(FieldList, lists:seq(1, length(FieldList))),
  case lists:keysearch(Field, 1, FieldPositions) of
    {value, {Field, FieldPosition}} -> 
      [Row || Row <- Rows, constraint_accepts(Constraint, lists:nth(FieldPosition, Row), Value)];
    false                           -> 
      throw(lists:flatten(io_lib:format("Invalid field ~s specified in constraint", [Field])))
  end.

constraint_accepts(eq, Value, Expected) ->
  bql_utils:convert_to_string(Value) =:= Expected;
constraint_accepts(neq, Value, Expected) ->
  not(bql_utils:convert_to_string(Value) =:= Expected);
constraint_accepts(lt, Value, Expected) ->
  {IntExpected, _Rest} = string:to_integer(Expected),
  Value < IntExpected;
constraint_accepts(lteq, Value, Expected) ->
  {IntExpected, _Rest} = string:to_integer(Expected),
  Value =< IntExpected;
constraint_accepts(gt, Value, Expected) ->
  {IntExpected, _Rest} = string:to_integer(Expected),
  Value > IntExpected;
constraint_accepts(gteq, Value, Expected) ->
  {IntExpected, _Rest} = string:to_integer(Expected),
  Value >= IntExpected;
constraint_accepts(like, Value, Expected) ->
  % Build the REs
  {ok, PeriodReplaceRe} = re:compile("\\."),
  {ok, PercentReplaceRe} = re:compile("%"),

  % Update the pattern
  ProtectedPeriods = re:replace(Expected, PeriodReplaceRe, "\\\\.", [global, {return, list}]),
  PercentagesAsWildcards = re:replace(ProtectedPeriods, PercentReplaceRe, ".*", [global, {return, list}]),
  
  % Compile the user pattern
  {ok, LikePattern} = re:compile("^" ++ PercentagesAsWildcards ++ "$"),

  % Test if the pattern matches
  case re:run(Value, LikePattern) of
    {match, _} -> true;
    nomatch -> false
  end.

apply_ordering(_FieldList, Rows, none) ->
  Rows;
apply_ordering(FieldList, Rows, {order_by, Clauses}) ->
  FieldPositions = lists:zip(FieldList, lists:seq(1, length(FieldList))),
  OrderingFieldPositions = [{name_to_position(Name, FieldPositions), Direction} || {Name, Direction} <- Clauses],
  lists:sort(fun(Row1, Row2) -> order_items(Row1, Row2, OrderingFieldPositions) end, Rows).

name_to_position(Field, FieldPositions) ->
  case lists:keysearch(Field, 1, FieldPositions) of
    {value, {Field, FieldPosition}} ->
      FieldPosition;
    false ->
      throw(lists:flatten(io_lib:format("Invalid field ~s specified in ordering clause", [Field])))
  end.

order_items(_, _, []) ->
  true;
order_items(Row1, Row2, [{FieldPosition, Direction} | RestOrdering]) ->
  Row1Val = lists:nth(FieldPosition, Row1),
  Row2Val = lists:nth(FieldPosition, Row2),
  case Row1Val == Row2Val of
    true -> order_items(Row1, Row2, RestOrdering);
    false ->
      case Direction of
        descending -> Row1Val > Row2Val;
        ascending  -> Row1Val < Row2Val
      end
  end.

filter_cols(AllFields, RequiredFields, Rows) ->
  FieldPositions = lists:zip(AllFields, lists:seq(1, length(AllFields))),
  Extract = fun(Field, Row) ->
     {value, {_, Position}} = lists:keysearch(Field, 1, FieldPositions),
     lists:nth(Position, Row)
  end,
  [[Extract(Field, Row) || Field <- RequiredFields] || Row <- Rows].

validate_fields(Available, Requested) ->
	case Requested of
		all   -> Available;
		_     ->
  			AvailableSet = sets:from_list(Available),
			RequestedSet = sets:from_list(Requested),
			Invalid = sets:subtract(RequestedSet, AvailableSet),
			case sets:size(Invalid) of
				0 -> Requested;
				1 -> throw(lists:flatten(io_lib:format("The field ~p is invalid", sets:to_list(Invalid))));
				_ -> throw(lists:flatten(io_lib:format("The fields ~p are invalid", [sets:to_list(Invalid)])))
			end
	end.

% Privilege Helpers
expand_privilege_list(all) ->
  [configure, read, write];
expand_privilege_list(X) ->
  [X].

apply_privilege_list(Node, User, PrivilegeList, Regex) ->
  % Retrieve the old privilege structure
  Current = retrieve_privileges(Node, User),

  % Update each privilege detailed in the privilege spec
  NewPrivs = [case X of
                {PrivKey, CurVal} ->
                    case lists:member(PrivKey, PrivilegeList) of
                        true -> Regex;
                        false -> CurVal
                    end
              end || X <- Current],
  [NewConfigure,NewWrite,NewRead] = NewPrivs,

  % Set the permissions
  rpc_call(Node, rabbit_access_control, set_permissions, [User, <<"/">>, NewConfigure, NewWrite, NewRead]),
  ok.

retrieve_privileges(Node, User) ->
  Permissions = rpc_call(Node, rabbit_access_control, list_vhost_permissions, [<<"/">>]),
  UserPermissions = [[{configure, ConfigureRE}, {write, WriteRE}, {read, ReadRE}]
    || {PermUser, ConfigureRE, WriteRE, ReadRE} <- Permissions, User =:= PermUser],
  case length(UserPermissions) of
    0 -> [{configure, <<"">>}, {write, <<"">>}, {read, <<"">>}];
    _ -> lists:nth(1, UserPermissions)
  end.

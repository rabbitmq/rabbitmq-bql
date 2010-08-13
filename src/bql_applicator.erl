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
-module(bql_applicator).

-export([apply_commands/3]).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-define(RPC_TIMEOUT, 30000).
-define(MASTER_VHOST, <<"/">>).

-record(state, {node, user, vhost}).

apply_commands(Commands, User, VHost) ->
    % Create a connection to the Rabbit node
    Node = rabbit_misc:makenode(node()),

    {ok, [catch apply_command(Command, #state {node = Node, user = User, vhost = VHost}) 
            || Command <- Commands]}.
                                          
% Queue Management
apply_command({create_queue, Name, Durable, Args}, #state {user = Username, vhost = VHost}) ->
    QueueName = rabbit_misc:r(VHost, queue, list_to_binary(Name)),
    ensure_resource_access(Username, QueueName, configure),
    rabbit_amqqueue:declare(QueueName, Durable, false, Args, none),
    ok;
apply_command({drop_queue, Name}, #state {user = Username, vhost = VHost}) ->
    QueueName = rabbit_misc:r(VHost, queue, list_to_binary(Name)),
    ensure_resource_access(Username, QueueName, configure),
    case rabbit_amqqueue:with(
           QueueName,
           fun (Q) -> rabbit_amqqueue:delete(Q, false, false) end) of
        {ok, _Purged} ->
            ok;
        {error, not_found} ->
            {error, io_lib:format("Queue ~s not found", [Name])}
    end;
apply_command({purge_queue, Name}, #state {user = Username, vhost = VHost}) ->
    QueueName = rabbit_misc:r(VHost, queue, list_to_binary(Name)),
    ensure_resource_access(Username, QueueName, read),
    rabbit_amqqueue:with_or_die(QueueName,
                                fun (Q) -> rabbit_amqqueue:purge(Q) end);

% Exchange Management
apply_command({create_exchange, Name, Type, Durable, Args}, #state {user = Username, vhost = VHost}) ->
    CheckedType = rabbit_exchange:check_type(list_to_binary(atom_to_list(Type))),
    ExchangeName = rabbit_misc:r(VHost, exchange, list_to_binary(Name)),
    ensure_resource_access(Username, ExchangeName, configure),
    X = case rabbit_exchange:lookup(ExchangeName) of
           {ok, FoundX} -> FoundX;
           {error, not_found} ->
               case rabbit_misc:r_arg(VHost, exchange, Args,
                                      <<"alternate-exchange">>) of
                   undefined -> ok;
                   AName     -> ensure_resource_access(Username, ExchangeName, read),
                                ensure_resource_access(Username, AName, write),
                                ok
               end,
               rabbit_exchange:declare(ExchangeName, CheckedType,
                                       Durable, false, Args)
       end,
    ok = rabbit_exchange:assert_equivalence(X, CheckedType, Durable,
                                            false, Args),
    ok;
apply_command({drop_exchange, Name}, #state {user = Username, vhost = VHost}) ->
    ExchangeName = rabbit_misc:r(VHost, exchange, list_to_binary(Name)),
    ensure_resource_access(Username, ExchangeName, configure),
    case rabbit_exchange:delete(ExchangeName, false) of
        {error, not_found} ->
            io_lib:format("Unknown exchange ~s", [Name]);
        ok ->
            ok
    end;

% User Management
apply_command({create_user, Name, Password}, #state {user = Username, node = Node}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    rpc_call(Node, rabbit_access_control, add_user, [list_to_binary(Name), list_to_binary(Password)]),
    ok;
apply_command({drop_user, Name}, #state {user = Username, node = Node}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    rpc_call(Node, rabbit_access_control, delete_user, [list_to_binary(Name)]),
    ok;

% VHost Management
apply_command({create_vhost, Name}, #state {user = Username, node = Node}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    rpc_call(Node, rabbit_access_control, add_vhost, [list_to_binary(Name)]),
    ok;
apply_command({drop_vhost, Name}, #state {user = Username, node = Node}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    rpc_call(Node, rabbit_access_control, delete_vhost, [list_to_binary(Name)]),
    ok;

% Binding Management
apply_command({create_binding, {X, Q, RoutingKey}, Args}, #state {user = Username, vhost = VHost}) ->
    binding_action(fun rabbit_exchange:add_binding/5, 
                   list_to_binary(X), list_to_binary(Q),
                   list_to_binary(RoutingKey), Args, Username, VHost);
apply_command({drop_binding, {X, Q, RoutingKey}}, #state {user = Username, vhost = VHost}) ->
    binding_action(fun rabbit_exchange:delete_binding/5, 
                   list_to_binary(X), list_to_binary(Q),
                   list_to_binary(RoutingKey), <<"">>, Username, VHost);

% Privilege Management
apply_command({grant, Privilege, Regex, User}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    PrivilegeList = expand_privilege_list(Privilege),
    apply_privilege_list(list_to_binary(User), VHost, PrivilegeList, list_to_binary(Regex));
apply_command({revoke, Privilege, User}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, configure),
    PrivilegeList = expand_privilege_list(Privilege),
    apply_privilege_list(list_to_binary(User), VHost, PrivilegeList, <<"">>);
  
% Queries
apply_command({select, "exchanges", Fields, Modifiers}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, VHost, read),
    AllFieldList = rabbit_exchange:info_keys(),
    FieldList = validate_fields(AllFieldList, Fields),
    Exchanges = rpc_call(Node, rabbit_exchange, info_all, [VHost]),
    interpret_response(AllFieldList, FieldList, Exchanges, Modifiers);

apply_command({select, "queues", Fields, Modifiers}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, VHost, read),
    AllFieldList = rabbit_amqqueue:info_keys(),
    FieldList = validate_fields(AllFieldList, Fields),
    Queues = rpc_call(Node, rabbit_amqqueue, info_all, [VHost]),
    interpret_response(AllFieldList, FieldList, Queues, Modifiers);

apply_command({select, "bindings", Fields, Modifiers}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, VHost, read),
    AllFieldList = [exchange_name, queue_name, routing_key, args],
    FieldList = validate_fields(AllFieldList, Fields),
    Bindings = rpc_call(Node, rabbit_exchange, list_bindings, [VHost]),
    interpret_response(AllFieldList, FieldList, Bindings, Modifiers);

apply_command({select, "users", Fields, Modifiers}, #state {node = Node, user = Username}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, read),
    AllFieldList = [name],
    FieldList = validate_fields(AllFieldList, Fields),
    Response = rpc_call(Node, rabbit_access_control, list_users, []),
    Users = [[binary_to_list(User)] || User <- Response],
    interpret_response(AllFieldList, FieldList, Users, Modifiers);

apply_command({select, "vhosts", Fields, Modifiers}, #state {node = Node, user = Username}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, read),
    AllFieldList = [name],
    FieldList = validate_fields(AllFieldList, Fields),
    Response = rpc_call(Node, rabbit_access_control, list_vhosts, []),
    VHosts = [[{name, binary_to_list(User)}] || User <- Response],
    interpret_response(AllFieldList, FieldList, VHosts, Modifiers);

apply_command({select, "permissions", Fields, Modifiers}, #state {node = Node, user = Username, vhost = VHost}) ->
    ensure_wildcard_access(Username, VHost, read),
    AllFieldList = [username,configure_perm,write_perm,read_perm],
    FieldList = validate_fields(AllFieldList, Fields),
    Permissions = rpc_call(Node, rabbit_access_control, list_vhost_permissions, [VHost]),
    interpret_response(AllFieldList, FieldList, Permissions, Modifiers);

apply_command({select, "connections", Fields, Modifiers}, #state {node = Node, user = Username}) ->
    ensure_wildcard_access(Username, ?MASTER_VHOST, read),
    AllFieldList = [pid, address, port, peer_address, peer_port, recv_oct, recv_cnt, send_oct, send_cnt,
                    send_pend, state, channels, user, vhost, timeout, frame_max],
    FieldList = validate_fields(AllFieldList, Fields),
    Connections = rpc_call(Node, rabbit_networking, connection_info_all, []),
    interpret_response(AllFieldList, FieldList, Connections, Modifiers);

% Sending Messages
apply_command({post_message, X, RoutingKey, Msg}, #state { user = Username, vhost = VHost }) ->
    ExchangeName = rabbit_misc:r(VHost, exchange, list_to_binary(X)),
    ensure_resource_access(Username, ExchangeName, write),
    Exchange = rabbit_exchange:lookup_or_die(ExchangeName),
    Message = rabbit_basic:message(ExchangeName, list_to_binary(RoutingKey),
                                   #'P_basic'{}, list_to_binary(Msg)),
    {RoutingRes, _DeliveredQPids} =
                rabbit_exchange:publish(
                  Exchange,
                  rabbit_basic:delivery(true, false, none, Message)),
    case RoutingRes of
        routed ->
            ok;
        unroutable ->
            "Message was unroutable";
        not_delivered ->
            "Message was not able to be delivered"
    end;
    
% Retreving Messages
apply_command({retrieve_message, QName}, State = #state{}) ->
    with_queue(fun(Q) -> poll(Q) end, QName, State);

%% This drains messages to a file on the server
apply_command({drain_queue, QName}, State = #state{}) ->
    Fun = 
        fun(Q) ->
            case disk_log:open([{name, QName}, {type, halt}]) of
                {ok, Log} -> drain_loop(Q, Log);
                {repaired, Log, _, _} -> drain_loop(Q, Log);
                {error, Reason} ->
                    error_logger:error_msg("Could not open disk log for"
                                           " queue (~p): ~p~n", [Q, Reason]),
                    not_ok
            end,
            ok
        end,
  with_queue(Fun, QName, State);

apply_command({select, EntityName, _, _}, #state {}) ->
    lists:flatten("Unknown entity " ++ EntityName ++ " specified to query");

% Catch-all  
apply_command(Unknown, _State) ->
    debug("Unknown command: ~p~n", [Unknown]).

drain_loop(Q, Log) ->
    case poll(Q) of
        empty -> ok;
        Payload ->
            case disk_log:blog(Log, Payload) of
                ok -> drain_loop(Q, Log);
                _ -> ok
            end
    end,
    disk_log:close(Log),
    ok.

with_queue(Fun, Queue, #state{node = Node, user = Username, vhost = VHost}) ->
    QueueName = rabbit_misc:r(VHost, queue, list_to_binary(Queue)),
    ensure_resource_access(Username, QueueName, read),
    
    case rpc_call(Node, rabbit_amqqueue, lookup, [QueueName]) of
        {error, not_found} -> 
            lists:flatten(io_lib:format("~s not found", [rabbit_misc:rs(QueueName)]));
        {ok, Q} -> 
            Fun(Q)
    end.

poll(Q) ->
    case rabbit_amqqueue:basic_get(Q, self(), true) of
        {ok, _MsgCount,
         {_QName, _QPid, _MsgId, _Redelivered,
          #basic_message{content = Content}}} ->
            {_Props, Payload} = rabbit_basic:from_content(Content),
            Payload;
        empty ->
            empty
    end.

% Debug Control
debug(_Format, _Params) ->
    ok.
    %% io:format(Format, Params).

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
    Extract = 
        fun(Field, Row) ->
            {value, {_, Position}} = 
                lists:keysearch(Field, 1, FieldPositions),
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

apply_privilege_list(User, VHost, PrivilegeList, Regex) ->
    %% Retrieve the old privilege structure
    Current = retrieve_privileges(User, VHost),

    %% Update each privilege detailed in the privilege spec
    NewPrivs = [case X of
                {PrivKey, CurVal} ->
                    case lists:member(PrivKey, PrivilegeList) of
                        true -> Regex;
                        false -> CurVal
                    end
              end || X <- Current],
    [NewConfigure,NewWrite,NewRead] = NewPrivs,

    % Set the permissions
    rabbit_access_control:set_permissions(User, VHost, NewConfigure, NewWrite, NewRead),
    ok.

retrieve_privileges(User, VHost) ->
    Permissions = rabbit_access_control:list_vhost_permissions(VHost),
    UserPermissions = [[{configure, ConfigureRE}, {write, WriteRE}, {read, ReadRE}]
        || {PermUser, ConfigureRE, WriteRE, ReadRE, _Scope} <- Permissions, User =:= PermUser],
    case length(UserPermissions) of
        0 -> [{configure, <<"">>}, {write, <<"">>}, {read, <<"">>}];
        _ -> lists:nth(1, UserPermissions)
    end.

ensure_resource_access(Username, Resource, Perm) ->
    rabbit_access_control:check_resource_access(Username, Resource, Perm).
    
ensure_wildcard_access(Username, VHost, Perm) ->
    VHostPerms = retrieve_privileges(Username, VHost),
    Priv = proplists:get_value(Perm, VHostPerms, <<"">>),
    case Priv == <<".*">> of
        true -> ok;
        false -> 
            rabbit_misc:protocol_error(access_refused, 
                "wildcard access to ~p on vhost ~s refused for user '~s'",
                [Perm, VHost, Username])
    end.
    
binding_action(Fun, ExchangeNameBin, QueueNameBin, RoutingKey, Arguments, Username, VHost) ->
    QueueName = rabbit_misc:r(VHost, queue, QueueNameBin),
    ensure_resource_access(Username, QueueName, write),
    ExchangeName = rabbit_misc:r(VHost, exchange, ExchangeNameBin),
    ensure_resource_access(Username, ExchangeName, read),
    case Fun(ExchangeName, QueueName, RoutingKey, Arguments, fun (_X, _Q) -> ok end) of
        {error, exchange_not_found} ->
            lists:flatten(io_lib:format("~s not found", [rabbit_misc:rs(ExchangeName)]));
        {error, queue_not_found} ->
            lists:flatten(io_lib:format("~s not found", [rabbit_misc:rs(QueueName)]));
        {error, exchange_and_queue_not_found} ->
            lists:flatten(io_lib:format("Neither ~s nor ~s exist",
                                        [rabbit_misc:rs(ExchangeName), 
                                         rabbit_misc:rs(QueueName)]));
        {error, binding_not_found} ->
            ok;
        {error, durability_settings_incompatible} ->
            lists:flatten(io_lib:format("Durability settings of ~s incompatible with ~s",
                                        [rabbit_misc:rs(QueueName), 
                                         rabbit_misc:rs(ExchangeName)]));
        ok -> ok
    end.

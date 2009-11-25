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
-module(bql_utils).

-export([convert_to_string/1, makenode/1]).

convert_to_string(Value) when is_list(Value) ->
    Value;
convert_to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
convert_to_string(Value) ->
    io_lib:write(Value).

%% Imported from rabbit_misc to remove the dependency on the Rabbit server!
makenode({Prefix, Suffix}) ->
    list_to_atom(lists:append([Prefix, "@", Suffix]));
makenode(NodeStr) ->
    makenode(nodeparts(NodeStr)).

nodeparts(Node) when is_atom(Node) ->
    nodeparts(atom_to_list(Node));
nodeparts(NodeStr) ->
    case lists:splitwith(fun (E) -> E =/= $@ end, NodeStr) of
        {Prefix, []}     -> {_, Suffix} = nodeparts(node()),
                            {Prefix, Suffix};
        {Prefix, Suffix} -> {Prefix, tl(Suffix)}
    end.

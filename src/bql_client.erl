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

% Client application for executing BDL commands.

-export([start/0, stop/0]).

start() ->
    FullCommand = init:get_plain_arguments(),
    case FullCommand of
      [] ->
         execute_shell(),
         halt();
      [BQL] ->
         case apply_bql_file(BQL) of
           ok    -> halt();
           error -> halt(1)
         end;
      _ ->
         io:fwrite("Too many arguments supplied. Provide a BDL file that should be applied.~n"),
         halt()
    end.

stop() ->
    ok.

execute_shell() ->
    case run_command() of
        exit -> ok;
        _    -> execute_shell()
    end.

run_command() ->
    Line = io:get_line("BQL> "),
    case Line of
        eof      -> exit;
        "exit\n" -> exit;
        _        -> execute_block(Line), ok
    end.
      

apply_bql_file(BQL) ->
    case filelib:is_file(BQL) of
        false ->
            io:fwrite("Provided BQL file does not exist!~n"),
            error;
        true ->
            {ok, Contents} = file:read_file(BQL),
            execute_block(binary_to_list(Contents))
    end.

execute_block(Contents) ->
    case rpc:call(localnode(rabbit), bql_server, send_command, 
                    [<<"guest">>, <<"guest">>, <<"text/bql">>, Contents]) of	
        {ok, Result}    -> format_result(Result);
        {error, Reason} -> io:format("BQL execution failed:~n  ~s~n", [Reason])
    end.

format_result(Result) ->
    [format_result_block(Item) || Item <- Result],
    ok.

format_result_block({Headers, Rows}) when is_list(Headers), is_list(Rows) ->
    %% Convert the content of all the rows to strings
    StringifiedRows = [[bql_utils:convert_to_string(Cell) || Cell <- Row] || Row <- Rows],

    %% Work through the items and headers, and find the longest item
    CountedHeaders = lists:zip(Headers, lists:seq(1, length(Headers))),
    Widths = [measure_column(Header, Position, StringifiedRows) || {Header, Position} <- CountedHeaders],

    %% Output the header then inside dividers
    Divider = ["-" || _ <- lists:seq(1, lists:sum(Widths) + 3*length(Widths) + 1)] ++ "~n",
    io:fwrite(Divider),
    output_row([atom_to_list(H) || H <- Headers], Widths),
    io:fwrite(Divider),

    [output_row(Row, Widths) || Row <- StringifiedRows],
    io:fwrite("~n"),
    ok;
format_result_block(Result) ->
    io:format("~p~n", [Result]),
    ok.

measure_column(Header, Position, Items) ->
    lists:max([length(X) || X <- [atom_to_list(Header)] ++ [lists:nth(Position, Row) || Row <- Items]]).
output_row(Items, Widths) ->
    WidthItems = lists:zip(Items, Widths),
    [io:format("| ~s ", [widen(Item, Width)]) || {Item, Width} <- WidthItems],
    io:fwrite("|~n").

widen(Item, Width) ->
    Extra = Width - length(Item),
    case Extra of
        0 -> Item;
        _ -> Item ++ [" " || _ <- lists:seq(1, Extra)]
    end.

localnode(Name) ->
    %% Imported from rabbit_misc to remove the dependency on the Rabbit server!
    %% This is horrible, but there doesn't seem to be a way to split a
    %% nodename into its constituent parts.
    list_to_atom(lists:append(atom_to_list(Name),
                              lists:dropwhile(fun (E) -> E =/= $@ end,
                                              atom_to_list(node())))).

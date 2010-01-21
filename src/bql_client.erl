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

% Record defining the context in which BQL commands are executed
-record(client_ctx, {username, password, vhost}).

start() ->
    Username = list_to_binary(argument_or_default(username, "guest")),
    Password = list_to_binary(argument_or_default(password, "guest")),
    VHost = list_to_binary(argument_or_default(vhost, "/")),
    ClientContext = #client_ctx{username = Username, password = Password, vhost = VHost},

    case init:get_argument(execute) of
      error ->
         execute_shell(ClientContext),
         halt();
      {ok, BQL} ->
         case apply_bql_file(ClientContext, BQL) of
           ok    -> halt();
           error -> halt(1)
         end;
      _ ->
         io:fwrite("Too many arguments supplied. Provide a BQL file that should be applied.~n"),
         halt()
    end.

stop() ->
    ok.

argument_or_default(Flag, Default) ->
  case init:get_argument(Flag) of
    {ok, [[Val]]} -> Val;
    _ -> Default
  end.

execute_shell(ClientContext) ->
    case run_command(ClientContext) of
        exit -> ok;
        _    -> execute_shell(ClientContext)
    end.

run_command(ClientContext) ->
    Line = io:get_line("BQL> "),
    case Line of
        eof      -> exit;
        "exit\n" -> exit;
        _        -> execute_block(ClientContext, Line), ok
    end.
      

apply_bql_file(ClientContext, BQL) ->
    case filelib:is_file(BQL) of
        false ->
            io:fwrite("Provided BQL file does not exist!~n"),
            error;
        true ->
            {ok, Contents} = file:read_file(BQL),
            execute_block(ClientContext, binary_to_list(Contents))
    end.

execute_block(#client_ctx { username = User, password = Password, vhost = VHost }, Contents) ->
    case rpc:call(bql_utils:makenode("rabbit"), bql_server, send_command, 
                    [User, Password, VHost, <<"text/bql">>, Contents]) of	
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

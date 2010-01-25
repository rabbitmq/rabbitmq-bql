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
-module(commands).

-export([parse/1,parse/2]).

-define(LEXER,command_lexer).
-define(PARSER,command_parser).

parse(String) ->
    case ?LEXER:string(String) of
        {ok,Tokens,_EndLine}  ->
            case ?PARSER:parse(Tokens) of
                {ok, Commands} -> {ok, Commands};
                {error, {Line, _Module, Reason}} -> {error, lists:flatten(io_lib:format("~s on line ~p", [Reason, Line]))}
            end;
        {error, {Line, _Lexer, {illegal, Token}}, _Line} -> {error, lists:flatten(io_lib:format("Illegal token ~p on line ~p", [Token, Line]))}
    end.

parse(file, FileName) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop(InFile,[]),
    file:close(InFile),
    ?PARSER:parse(Acc).

loop(InFile,Acc) ->
    case io:request(InFile, {get_until,prompt,?LEXER,token,[1]}) of
        {ok,Toks,_EndLine} ->
            loop(InFile,Acc ++ [Toks]);
        {error,token} ->
            exit("Scanning error");
        {eof,_} ->
            Acc
    end.


@echo off
REM   The contents of this file are subject to the Mozilla Public License
REM   Version 1.1 (the "License"); you may not use this file except in
REM   compliance with the License. You may obtain a copy of the License at
REM   http://www.mozilla.org/MPL/
REM
REM   Software distributed under the License is distributed on an "AS IS"
REM   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
REM   License for the specific language governing rights and limitations
REM   under the License.
REM
REM   The Original Code is RabbitMQ BQL Plugin.
REM
REM   The Initial Developers of the Original Code are LShift Ltd.
REM
REM   Copyright (C) 2009 LShift Ltd.
REM
REM   All Rights Reserved.
REM
REM   Contributor(s): ______________________________________.
REM

if "%ERLANG_HOME%"=="" (
    set ERLANG_HOME=%~dp0%..\..\..
)

if not exist "%ERLANG_HOME%\bin\erl.exe" (
    echo.
    echo ******************************
    echo ERLANG_HOME not set correctly. 
    echo ******************************
    echo.
    echo Please either set ERLANG_HOME to point to your Erlang installation or place the
    echo RabbitMQ server distribution in the Erlang lib folder.
    echo.
    exit /B
)

"%ERLANG_HOME%\bin\erl.exe" -pa "%~dp0..\ebin" -noshell -hidden -sname amqpbql -s bql_dump -extra %*


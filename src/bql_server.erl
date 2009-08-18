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
-module(bql_server).

-behaviour(gen_server).

-export([start/0, start/2, stop/0, stop/1, start_link/0, send_command/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start() ->
    start_link(),
    ok.

start(normal, []) ->
    start_link().

stop() ->
    ok.

stop(_State) ->
    stop().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_command(Username, Password, ContentType, Command) ->
    gen_server:call(?MODULE, {execute, Username, Password, ContentType, Command}).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
    {ok, #state{}}.

handle_call(Msg,_From,State = #state{}) ->
    case catch handle_message(Msg) of
        {reply, Content} ->
            {reply, Content, State};
        {'EXIT', {amqp,access_refused,ErrorMsg,none}} ->
            {reply, {error, ErrorMsg}, State};
        {'EXIT', Reason} ->
            {reply, {error, Reason}, State};
        Response ->
            {reply, {error, Response}, State}
    end.

handle_cast(_,State) -> 
    {noreply, State}.
    
handle_info(_Info, State) -> 
    {noreply, State}.
    
terminate(_,_) -> 
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% Message Handling
handle_message({execute, Username, Password, ContentType, Command}) ->
    %% Validate the user credentials
    rabbit_access_control:user_pass_login(Username, Password),
    
    %% Parse the input based on the content type
    ParsedCommands = case ContentType of
        <<"text/bql">> ->
            commands:parse(Command);
        <<"application/bql-terms">> ->
            list_to_term(Command)
    end,
    
    % rabbit_access_control:check_resource_access(Username, Resource, Perm)
    
    case ParsedCommands of
        {ok, Commands} ->
            case bql_applicator:apply_commands(Commands, Username) of
                {ok, Result} ->
                    {reply, {ok, Result}};
                {error, Reason} ->
                    {reply, {error, Reason}}
            end;
        {error, Reason} ->
            {reply, {error, Reason}}
    end;
handle_message(_) ->
    {reply, unknown_command}.

%%
%% Helper Methods
%%

list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    erl_parse:parse_term(T).

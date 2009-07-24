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

-export([start/0, start/2, stop/0, stop/1, start_link/0, send_command/3]).
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
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

send_command(Username, Password, Command) ->
  gen_server:call({global, ?MODULE}, {execute, Username, Password, Command}).

%---------------------------
% Gen Server Implementation
% --------------------------

init([]) ->
  {ok, #state{}}.

handle_call(Msg,_From,State = #state{}) ->
  case Msg of
    {execute, _Username, _Password, Command} ->
      % rabbit_access_control:user_pass_login(Username, Password),

      % rabbit_access_control:check_resource_access(Username, Resource, Perm)
      % select name,depth from queues where name = 'amq.control'; => [{name, depth}, {"amq.control", 5}]

     case commands:parse(Command) of
        {ok, Commands} ->
          case bql_applicator:apply_commands(Commands) of
            {ok, Result} ->
              {reply, {ok, Result}, State};
            {error, Reason} ->
              {reply, {error, Reason}, State}
          end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
     end;
    _ ->
      {reply, unknown_command, State}
  end. 

handle_cast(_,State) -> {reply,unhandled_cast,State}.
handle_info(_Info, State) -> {reply, unhandled_info, State}.
terminate(_,_) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

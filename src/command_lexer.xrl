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
% Lexes a selector

Definitions.

C     = [0-9A-Za-z\.\-\_]
W     = [0-9A-Za-z\s\.\-\_%\/*#@]
Semi  = [;]
Wild  = [*]
Comma = [,]
WS    = ([\000-\s]|%.*)
Comp  = (<|<=|=|=>|>|!=|<>)


Rules.
create      :   {token,{create,TokenLine,list_to_atom(TokenChars)}}.
drop        :   {token,{drop,TokenLine,list_to_atom(TokenChars)}}.
durable     :   {token,{durable,TokenLine,list_to_atom(TokenChars)}}.
queue       :   {token,{queue,TokenLine,list_to_atom(TokenChars)}}.
vhost       :   {token,{vhost,TokenLine,list_to_atom(TokenChars)}}.
exchange    :   {token,{exchange,TokenLine,list_to_atom(TokenChars)}}.
direct      :   {token,{exchange_type,TokenLine,list_to_atom(TokenChars)}}.
headers     :   {token,{exchange_type,TokenLine,list_to_atom(TokenChars)}}.
fanout      :   {token,{exchange_type,TokenLine,list_to_atom(TokenChars)}}.
topic       :   {token,{exchange_type,TokenLine,list_to_atom(TokenChars)}}.
route       :   {token,{route,TokenLine,list_to_atom(TokenChars)}}.
from        :   {token,{from,TokenLine,list_to_atom(TokenChars)}}.
to          :   {token,{to,TokenLine,list_to_atom(TokenChars)}}.
on          :   {token,{on,TokenLine,list_to_atom(TokenChars)}}.
routing_key :   {token,{routing_key,TokenLine,list_to_atom(TokenChars)}}.
identified  :   {token,{identified,TokenLine,list_to_atom(TokenChars)}}.
by          :   {token,{by,TokenLine,list_to_atom(TokenChars)}}.
is          :   {token,{is,TokenLine,list_to_atom(TokenChars)}}.
when        :   {token,{when_sym,TokenLine,list_to_atom(TokenChars)}}.
user        :   {token,{user,TokenLine,list_to_atom(TokenChars)}}.
select      :   {token,{select,TokenLine,list_to_atom(TokenChars)}}.
where       :   {token,{where,TokenLine,list_to_atom(TokenChars)}}.
and         :   {token,{union,TokenLine,and_sym}}.
or          :   {token,{union,TokenLine,or_sym}}.
order       :   {token,{order,TokenLine,list_to_atom(TokenChars)}}.
by          :   {token,{by,TokenLine,list_to_atom(TokenChars)}}.
asc         :   {token,{asc,TokenLine,list_to_atom(TokenChars)}}.
desc        :   {token,{desc,TokenLine,list_to_atom(TokenChars)}}.
grant       :   {token,{grant,TokenLine,list_to_atom(TokenChars)}}.
revoke      :   {token,{revoke,TokenLine,list_to_atom(TokenChars)}}.
purge       :   {token,{purge,TokenLine,list_to_atom(TokenChars)}}.
post        :   {token,{post,TokenLine,list_to_atom(TokenChars)}}.
with        :   {token,{with,TokenLine,list_to_atom(TokenChars)}}.
get         :   {token,{get,TokenLine,list_to_atom(TokenChars)}}.
{Wild}      :   {token,{wildcard,TokenLine,list_to_atom(TokenChars)}}.
{Comma}     :   {token,{comma,TokenLine,list_to_atom(TokenChars)}}.
{Comp}      :   {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.
like        :   {token,{comparator,TokenLine,list_to_atom(TokenChars)}}.
'{W}*'      :   {token,{string,TokenLine,strip(TokenChars, TokenLen)}}.
{C}+        :   {token,{string,TokenLine,TokenChars}}.
{Semi}      :   {token,{semi,TokenLine,list_to_atom(TokenChars)}}.
{WS}+       :   skip_token.

Erlang code.
strip(TokenChars,TokenLen) -> lists:sublist(TokenChars, 2, TokenLen - 2).

%%
%% %CopyrightBegin%
%% 
%% Copyright Alfonso De Gregorio 2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
-module('idlexample_Hello_impl').


%% Interface functions
-export([ sayHello/0, 
          sayHello/1, 
          shutdown/0,
          shutdown/1]).

%% Interface with the erl_genserv backend
-export([init/1, terminate/2, start/0]).

init(_Env) ->
    {ok, []}.

terminate(_From, _Reason) ->
    ok.

%%%% Operation: sayHello
%%
%%   Returns: RetVal
%%
sayHello(_Random) ->
    sayHello().
sayHello() ->
    {reply, "Hello World!", []}.

%%%% Operation: shutdown
%%
%%   Returns: RetVal
%%
shutdown(_Random) ->
    shutdown().
shutdown() -> 
    ok = terminate(self(), timetoshutdown),
    {noreply, []}.


%% This starts up the random number server
start() ->
    %% Start the gen server
    {ok,_Pid} = 'idlexample_Hello':oe_create([],{local,'idlexample_Hello_impl'}),
    ok.

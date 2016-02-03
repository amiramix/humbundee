%% Copyright (c) 2016, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(cmd_config).

-export([
         subfolders/0,
         key_replace/4,
         process_config/4
        ]).

subfolders() ->
    {ok,
     [<<"downloads">>,
      <<"humblebundle">>,
      <<"mnesia_backups">>]}.

key_replace(Base, Name, Offset, RunVars) ->
    {_, Host} = proplists:get_value(hostname, RunVars),

    {ok,
     [
      {<<"=INETS_IP=">>, <<"0.0.0.0">>},
      {<<"=INETS_IP_TUPLE=">>, <<"{0,0,0,0}">>},
      {<<"=INETS_PORT=">>, integer_to_list(8080 + Offset), [global]},
      {<<"=HTTP_PORT=">>, integer_to_list(8380 + Offset)},
      {<<"=SERVICE_NAME=">>, Name},
      {<<"=HOSTNAME=">>, Host},
      {<<"=FINAL_ROOTDIR=">>, Base, [global]},
      {<<"=ROOT_DIR=">>, Base, [global]}
     ]}.

process_config(_App, _Dest, _CfgArgs, _Privs) ->
    false.

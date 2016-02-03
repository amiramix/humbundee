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

-module(hbd_setup).

-export([install/2]).

-include_lib("humbundee/include/humbundee.hrl").

log(F, A) -> io:format("~p:~p: " ++ F ++ "~n", [?MODULE, ?LINE] ++ A).

install({_Id, Node}, SetupCfg) ->
    Config = bld_cfg:load_config(SetupCfg),
    log("Config file: ~p~n", [Config]),

    ok = mnesia:create_schema([Node]),
    mnesia:start(),

    SetupApp = proplists:get_value(setup_app, SetupCfg),
    Src = proplists:get_value(hbd_mnesia_backup, Config),
    Source = filename:join(code:priv_dir(SetupApp), Src),
    case filelib:is_regular(Source) of
        false -> init_db(Node);
        true -> import_db(Source)
    end,
    ok.

init_db(Node) ->
    log("DB backup not found, initializing an empty DB.", []),
    Options = [{type, bag}, {attributes, record_info(fields, idx)},
               {disc_copies, [Node]}, {index, [sha1, md5]}],
    mnesia:create_table(idx, Options).

import_db(Source) ->
    log("Importing DB backup ~p", [Source]),
    Options = [{default_op, skip_tables}, {recreate_tables, [idx]}],
    {atomic, [idx]} = mnesia:restore(Source, Options).

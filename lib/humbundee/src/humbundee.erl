%% Copyright (c) 2015, Grzegorz Junka
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

-module(humbundee).

-export([download/0]).

download() ->
    try
        check_wget() andalso do()
    catch
        throw:Term -> {error, Term}
    end.

check_wget() ->
    case os:cmd("which wget") of
        [] -> no_wget();
        _ -> true
    end.

no_wget() ->
    yio:errorn(<<"Error: wget not installed.">>),
    throw(no_wget).

do() ->
    Cfg = read_config(),
    _Ids = read_ids(Cfg).

read_config() ->
    {ok, Url} = application:get_env(humbundee, order_url),
    {ok, Coo} = application:get_env(humbundee, cookie),
    {ok, Ids} = application:get_env(humbundee, order_ids_file),
    {ok, Dir} = application:get_env(humbundee, download_location),
    Exc = application:get_env(humbundee, exclude_regex_list, []),
    #{url => Url, cookie => Coo, ids => Ids, dir => Dir, regex => Exc}.

read_ids(#{ids := Ids} = Cfg) ->
    case file:read_file(Ids) of
        {ok, Bin} -> process_ids(Cfg, Bin);
        {error, Err} -> no_ids_file(Ids, Err)
    end.

no_ids_file(Ids, Err) ->
    yio:errorn(<<"Error: Could not read order ids from file '">>, Ids,
               <<"': ">>, Err),
    throw(no_ids).

process_ids(Cfg, Bin) ->
    Ids = binary:split(Bin, [<<"\n">>, <<"\r\n">>], [global]),
    Ids.

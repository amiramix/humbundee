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

-module(hbd_id).
-behaviour(gen_server).

%% API
-export([start_link/2,
         start_download/1]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("yolf/include/yolf.hrl").

-record(st, {id, url, cookie, regex, in, out}).

%%% API
start_link(Cfg, Id) ->
    ?LOG_WORKER(Id),
    gen_server:start_link(?MODULE, [Cfg, Id], []).

start_download(Pid) ->
    gen_server:cast(Pid, start).

%%% gen_server callbacks
init([Cfg, Id]) ->
    ?LOG_WORKER_INIT(Id),
    try
        {ok, #st{id = Id,
                 url    = maps:get(url, Cfg),
                 cookie = maps:get(cookie, Cfg),
                 regex  = maps:get(regex, Cfg),
                 in     = maps:get(in, Cfg),
                 out    = init_out_dir(maps:get(out, Cfg), Id)}}
    catch
        throw:Term -> {stop, Term}
    end.

handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast(start, State) ->
    start(State),
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal methods

init_out_dir(Dir, Id) ->
    Path = filename:join(Dir, Id),
    case filelib:is_file(Path) of
        false -> ensure_dir(Path, Id);
        true -> already_exists(Path)
    end,
    Path.

already_exists(Path) ->
    yio:en(<<"Error: The download path '">>, Path, <<"' already exists.">>),
    hbd_event:already_exists(Path),
    throw(already_exists).

ensure_dir(Path, Id) ->
    FileName = filename:join(Path, <<Id/binary, <<".log">>/binary>>),
    case filelib:ensure_dir(FileName) of
        ok -> start_log(FileName);
        {error, _} = Err -> bad_dir(Path, Err)
    end.

bad_dir(Path, Err) ->
    yio:en(<<"Can't create the download folder '">>, Path, <<"'.">>, endl,
           <<"Error: ">>, Err),
    hbd_event:bad_download_dir(Path, Err),
    throw(Err).

start_log(FileName) ->
    case yolog:init(FileName) of
        ok -> ok;
        Err -> no_log(FileName, Err)
    end.

no_log(FileName, Err) ->
    yio:en(<<"Can't open log file '">>, FileName, <<"'.">>, endl,
           <<"Error: ">>, Err),
    hbd_event:bad_log_path(FileName, Err),
    throw(Err).

%%------------------------------------------------------------------------------

start(#st{id = Id, url = Url, cookie = Cookie, regex = Regex, out = OutDir}) ->
    Data = hbd_json:process(Url, Id, Cookie, OutDir),
    yolog:in(<<"Ending process.">>).

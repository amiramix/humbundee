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
-export([
         start_link/2,
         start_download/1,
         torrent_added/2
        ]).

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

torrent_added(Pid, FileName) ->
    gen_server:cast(Pid, {added, FileName}).

%%% gen_server callbacks
init([Cfg, Id]) ->
    ?LOG_WORKER_INIT(Id),
    process_flag(trap_exit, true),
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

handle_call({started, Pid, Name}, _From, State) ->
    link(Pid),
    log_download(Pid, Name),
    {reply, ok, State};
handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast(start, State) ->
    start(State),
    {noreply, State};
handle_cast({log_torrent, Pid, Cmd, Result}, State) ->
    log_torrent(Pid, Cmd, Result),
    {noreply, State};
handle_cast({bad_torrent, Pid, Name}, State) ->
    log_bad_torrent(Pid, Name),
    {noreply, State};
handle_cast({excluded, Pid, Match, Subject}, State) ->
    log_excluded(Pid, Match, Subject),
    {noreply, State};
handle_cast({added, FileName}, State) ->
    log_torrent_added(FileName),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    log_exit(Pid, Reason),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal API
download_started(Pid, NewPid, Name) ->
    gen_server:call(Pid, {started, NewPid, Name}).

torrent_finished(Pid, NewPid, Cmd, Result) ->
    gen_server:cast(Pid, {log_torrent, NewPid, Cmd, Result}).

bad_torrent(Pid, NewPid, Name) ->
    gen_server:cast(Pid, {bad_torrent, NewPid, Name}).

found_excluded(Pid, NewPid, Match, Subject) ->
    gen_server:cast(Pid, {excluded, NewPid, Match, Subject}).

%%------------------------------------------------------------------------------

log_download(Pid, Name) ->
    yolog:tin(<<"Started pid: ">>, Pid, <<" to download: ">>, Name).

log_torrent(Pid, Cmd, ok) ->
    yolog:tin(<<"Success, process ">>, Pid,
              <<" downloaded torrent with command: ">>, endl, Cmd);
log_torrent(Pid, Cmd, Result) ->
    yolog:tin(<<"Error: ">>, Result, <<", process ">>, Pid,
              <<" couldn't download torrent with command: ">>, endl, Cmd).

log_exit(Pid, Reason) ->
    yolog:tin(<<"Process ">>, Pid, <<" terminated with reason ">>, Reason).

log_bad_torrent(Pid, Name) ->
    yolog:tin(<<"Error, process ">>, Pid,
              <<" can't find downloaded torrent ">>, Name).

log_excluded(Pid, Match, Subject) ->
    yolog:tin(<<"Process ">>, Pid, <<" finished, exluded pattern '">>, Match,
              <<"' found in subject '">>, Subject, <<"'.">>).

log_torrent_added(FileName) ->
    yolog:tin(<<"Torrent '">>, FileName, <<"' added to the download queue">>).

%%------------------------------------------------------------------------------
%% Internal methods

init_out_dir(Dir, Id) ->
    Path = filename:join(Dir, Id),
    case filelib:is_file(Path) of
        true -> already_exists(Path);
        false -> ensure_dir(Path, Id)
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

start(#st{id = Id, url = Url, cookie = Cookie, out = OutDir} = St) ->
    Data = hbd_json:process(Url, Id, Cookie, OutDir),
    TorrentDir = filename:join(OutDir, <<"torrents">>),
    ok = yocmd:mk_dir(TorrentDir),
    lists:foreach(fun(X) -> spawn_folder(TorrentDir, X, St) end, Data).

spawn_folder(TrDir, Data, St) ->
    Folder = filename:join(maps:get(folder, Data), maps:get(name, Data)),
    Fun = fun() -> start_folder(self(), TrDir, Folder, Data, St) end,
    Pid = proc_lib:spawn_link(Fun),
    log_download(Pid, Folder).

start_folder(Parent, TrDir, Folder, Data, St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    Downloads = maps:get(downloads, Data),
    Fun = fun(X) -> spawn_one(Parent, TrDir, Folder, X, St) end,
    lists:foreach(Fun, Downloads).

spawn_one(LogPid, TrDir, Folder, Download, St) ->
    Fun = fun() -> start_one(self(), LogPid, TrDir, Folder, Download, St) end,
    Pid = proc_lib:spawn_link(Fun),
    Name = maps:get(name, Download),
    ok = download_started(LogPid, Pid, filename:join(Folder, Name)),
    unlink(Pid).

start_one(Parent, LogPid, TrDir, Folder, Download, #st{regex = RegEx} = St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    Sha1 = maps:get(sha1, Download),
    Size = maps:get(size, Download),
    Name = maps:get(name, Download),
    Url = maps:get(url, Download),
    Torrent = maps:get(torrent, Download),
    exit_if_excluded(LogPid, RegEx, {Name, Url, Torrent}),
    TrCmd = torrent_cmd(TrDir, Torrent, St#st.cookie),
    start_torrent(LogPid, TrCmd, {Folder, Name, Sha1, Size}, St).

exit_if_excluded(LogPid, [{name, Re, Opts} | T], {Name, _, _} = Info) ->
    check_excluded(LogPid, Name, re:run(Name, Re, Opts)),
    exit_if_excluded(LogPid, T, Info);
exit_if_excluded(LogPid, [{link, Re, Opts} | T], {_, Url, Torrent} = Info) ->
    check_excluded(LogPid, Url, re:run(Url, Re, Opts)),
    check_excluded(LogPid, Torrent, re:run(Torrent, Re, Opts)),
    exit_if_excluded(LogPid, T, Info);
exit_if_excluded(LogPid, [{any, Re, Opts} | T], Info) ->
    Fun = fun(X) -> check_excluded(LogPid, X, re:run(X, Re, Opts)) end,
    lists:foreach(Fun, tuple_to_list(Info)),
    exit_if_excluded(LogPid, T, Info).

check_excluded(_LogPid, _Subject, nomatch) ->
    ok;
check_excluded(LogPid, Subject, {match, Captured}) ->
    found_excluded(LogPid, self(), Captured, Subject),
    exit(normal).

torrent_cmd(TrDir, Torrent, Cookie) ->
    {ok, {_, _, _, _, Path, _}} = http_uri:parse(Torrent),
    File = lists:last(filename:split(Path)),
    TorrentFile = filename:join(TrDir, File),
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -P ">>/binary, TrDir/binary, <<" ">>/binary, Torrent/binary >>,
    {Cmd, TorrentFile}.

start_torrent(LogPid, {Cmd, TrFile}, FileInfo, #st{in = InDir, out = OutDir}) ->
    case yexec:sh_cmd(Cmd) of
        {0, _} ->
            torrent_finished(LogPid, self(), Cmd, ok),
            add_torrent(LogPid, TrFile, {InDir, OutDir}, FileInfo);
        Err ->
            torrent_finished(LogPid, self(), Cmd, Err),
            exit(no_torrent)
    end.

add_torrent(LogPid, TrFile, InOut, FileInfo) ->
    case filelib:is_regular(TrFile) of
        true -> hbd_pool:add_torrent(LogPid, TrFile, InOut, FileInfo);
        false -> bad_torrent(LogPid, self(), TrFile)
    end.

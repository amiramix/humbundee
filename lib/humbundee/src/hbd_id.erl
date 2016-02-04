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

-module(hbd_id).
-behaviour(gen_server).

%% API
-export([
         start_link/3,
         start_download/1,
         download_added/3,
         file_started/2,
         warn_bad_sha1/2,
         stale_detected/2,
         download_completed/2,
         stale_error/4,
         status/1
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

-define(DATA_DIR, <<"_data">>).
-define(TORRENTS_DIR, <<"_torrents">>).

-include_lib("yolf/include/yolf.hrl").
-include("download.hrl").

-record(st, {id, mode, url, cookie, regex, out, pids, count, ends}).

%%% API
start_link(Cfg, Id, Mode) ->
    ?LOG_WORKER(Id),
    gen_server:start_link(?MODULE, [Cfg, Id, Mode], []).

start_download(Pid) ->
    gen_server:cast(Pid, start).

download_added(Pid, File, TrFile) ->
    gen_server:cast(Pid, {added, File, TrFile}).

file_started(Pid, File) ->
    gen_server:cast(Pid, {fetching, File}).

warn_bad_sha1(Pid, File) ->
    process_warning(Pid, bad_sha1, File).

stale_detected(Pid, File) ->
    gen_server:cast(Pid, {stale, File}).

download_completed(Pid, File) ->
    gen_server:cast(Pid, {downloaded, File}).

stale_error(Pid, File, Err, DelRes) ->
    gen_server:cast(Pid, {stale_err, File, Err, DelRes}).

status(Pid) ->
    gen_server:call(Pid, status).

%%------------------------------------------------------------------------------

%%% gen_server callbacks
init([Cfg, Id, Mode]) ->
    ?LOG_WORKER_INIT(Id),
    process_flag(trap_exit, true),
    try
        {ok, #st{id     = Id,
                 mode   = Mode,
                 url    = maps:get(url, Cfg),
                 cookie = maps:get(cookie, Cfg),
                 regex  = maps:get(regex, Cfg),
                 out    = init_out_dir(maps:get(dest, Cfg), Id),
                 pids   = sets:new(),
                 ends   = {0, 0, 0, 0}}}
    catch
        throw:Term -> {stop, Term}
    end.

handle_call({started, Pid, Name}, _From, St) ->
    log_download(Pid, Name),
    link(Pid),
    {reply, ok, St};
handle_call(status, _From, St) ->
    {reply, do_status(St), St};
handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast(start, #st{pids = Set} = St) ->
    List = start(St),
    Pids = sets:union(Set, sets:from_list([Pid || {Pid, _} <- List])),
    {noreply, St#st{pids = Pids, count = lists:sum([X || {_, X} <- List])}};
handle_cast({add, Pid}, #st{pids = Set} = St) ->
    {noreply, St#st{pids = sets:add_element(Pid, Set)}};
handle_cast({remove, Pid}, State) ->
    check_done(ok, Pid, State);
handle_cast({torrent, Pid, Cmd}, State) ->
    log_torrent(Pid, Cmd),
    {noreply, State};
handle_cast({added, File, TrFile}, State) ->
    log_torrent_added(File, TrFile),
    {noreply, State};
handle_cast({fetching, File}, State) ->
    log_fetching_file(File),
    {noreply, State};
handle_cast({stale, File}, State) ->
    log_stale_file(File),
    {noreply, State};
handle_cast({downloaded, File}, State) ->
    log_downloaded(File),
    {noreply, State};
handle_cast({done, Pid, Name}, State) ->
    log_done(Pid, Name),
    check_done(done, Pid, State);
handle_cast({excluded, Pid, Path, Match, Subject}, State) ->
    log_excluded(Pid, Path, Match, Subject),
    check_done(excluded, Pid, State);
handle_cast({ignored, Pid, Type, Args}, State) ->
    log_ignored(Type, Pid, Args),
    check_done(ignored, Pid, State);
handle_cast({warn, Type, Args}, State) ->
    log_warning(Type, Args),
    {noreply, State};
handle_cast({stale_err, File, Err, DelRes}, State) ->
    log_error(stale_err, {File, Err, DelRes}),
    {noreply, State};
handle_cast({error, Pid, Type, Args}, State) ->
    log_error(Type, Pid, Args),
    check_done(error, Pid, State);
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, noproc}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    log_exit(Pid, Reason),
    check_done(error, Pid, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%% Internal API
download_started(LPid, Pid, Name) ->
    gen_server:call(LPid, {started, Pid, Name}).


record_process(LPid) ->
    gen_server:cast(LPid, {add, self()}).

finish_process(LPid, Pid) ->
    gen_server:cast(LPid, {remove, Pid}).

torrent_finished(LPid, Cmd) ->
    gen_server:cast(LPid, {torrent, self(), Cmd}).

download_finished(LPid, Name) ->
    gen_server:cast(LPid, {done, self(), Name}),
    done.

found_excluded(LPid, Path, Match, Subject) ->
    gen_server:cast(LPid, {excluded, self(), Path, Match, Subject}).

process_ignored(LPid, Type, Args) ->
    gen_server:cast(LPid, {ignored, self(), Type, Args}).

process_warning(LPid, Type, Args) ->
    gen_server:cast(LPid, {warn, Type, Args}).

process_error(LPid, Type, Args) ->
    gen_server:cast(LPid, {error, self(), Type, Args}),
    error.

%%------------------------------------------------------------------------------

log_process(Pid, Line, Items) ->
    yolog:in([<<"Started pid ">>, Pid, <<" to process ">>, Items,
              <<" download(s) in line ">>, Line, <<".">>]).

log_download(Pid, Line) ->
    yolog:in(<<"Started pid: ">>, Pid, <<" to download: ">>, endl,
             <<"  [NEW_DOWN] ">>, Line).


log_torrent(Pid, Cmd) ->
    yolog:tin(<<"Success, process ">>, Pid,
              <<" downloaded torrent with command: ">>, endl,
              <<"  [TOR_DONE] ">>, Cmd).

log_torrent_added(Name, TrFile) ->
    yolog:tin([<<"Added to the download queue file/torrent:">>, endl,
               <<"  [QADDFILE] ">>, Name, endl,
               <<"  [QADD_TOR] ">>, TrFile]).

log_fetching_file(File) ->
    yolog:tin([<<"Started downloading file:">>, endl,
               <<"  [FILEDOWN] ">>, File]).

log_stale_file(File) ->
    yolog:tin([<<"Detected stale file, will try to download with 'wget':">>,
               endl, <<"  [STALEFIL] ">>, File]).

log_downloaded(File) ->
    yolog:tin([<<"Finished downloading file with 'wget':">>, endl,
               <<"  [WGETDONE] ">>, File]).


log_done(Pid, Name) ->
    yolog:tin(<<"Process ">>, Pid, <<" finished downloading: ">>, endl,
              <<"  [FILEDONE] ">>, Name).


log_excluded(Pid, Path, Match, Subject) ->
    yolog:tin([<<"Process ">>, Pid, <<" finished, excluded pattern '">>, Match,
               <<"' found in subject '">>, Subject, <<"', download:">>, endl,
               <<"  [EXCLDOWN] ">>, Path]).


log_ignored(in_idx, Pid, {Sha1, Md5, Path}) ->
    yolog:tin([<<"Process '">>, Pid, <<"' finished, entry with Sha1 '">>,
               Sha1, <<"' and/or Md5 '">>, Md5,
               <<"' was found in the index of downloaded files: ">>, endl,
               <<"  [IGN:IDXEXIST] ">>, Path]);
log_ignored(asmjs, Pid, Path) ->
    yolog:tin([<<"Process ">>, Pid, <<" finished, entry is an embedded ">>,
               <<"'asmjs' application and can't be downloaded: ">>, endl,
               <<"  [IGN:ASMJSAPP] ">>, Path]);
log_ignored(stream, Pid, Path) ->
    yolog:tin([<<"Process ">>, Pid, <<" finished, entry is a stream link ">>,
               <<"and can't be downloaded: ">>, endl,
               <<"  [IGN:__STREAM] ">>, Path]).


log_warning(no_torrent, {Cmd, Err}) ->
    yolog:tin([<<"Couldn't download torrent with command: ">>, endl,
               <<"  [WRN:TOR_DOWN] ">>, Cmd, endl, <<"reason: ">>, Err]);
log_warning(bad_torrent, {Name, Err}) ->
    yolog:tin([<<"Downloaded torrent file couldn't be parsed: ">>, endl,
               <<"  [WRN:BAD__TOR] ">>, Name, endl, <<"reason: ">>, Err]);
log_warning(torrent_size, {Name, Path, Size}) ->
    yolog:tin([<<"File size in the torrent ">>, Size,
               <<" differs from the size reported in JSON for:">>, endl,
               <<"  [WRN:SIZEFILE] ">>, Name, endl,
               <<"  [WRN:SIZEPATH] ">>, Path]);
log_warning(bad_sha1, File) ->
    yolog:tin([<<"Warning, incorrect SHA1 sum for file:">>, endl,
               <<"  [WRN:BAD_SHA1] ">>, File]).


log_error(Type, Pid, Args) ->
    yolog:tin([<<"Download error for ">>, Pid, <<": ">>, Type, endl]
              ++ log_error(Type, Args)).

log_error(mkdir, {Path, Err}) ->
    [<<"  Couldn't create the destination folder/torrent:">>, endl,
     <<"  [ERR:BAD_PATH] ">>, Path, endl, <<"reason: ">>, Err];
log_error(duplicate, {Name, Path, OtherRec}) ->
    [<<"  Duplicate file name, already downloading file/torrent:">>, endl,
     <<"  [ERR:DUPLFILE] ">>, Name, endl,
     <<"  [ERR:DUPLPATH] ">>, Path, endl,
     <<"  the other file already being downloaded:">>, endl,
     <<"  [ERR:DUPLOTHR] ">>, OtherRec];
log_error(torrent_cmd, {Name, Path, Err}) ->
    [<<"  Couldn't start bittorrent download:">>, endl,
     <<"  [ERR:BADCMDFI] ">>, Name, endl,
     <<"  [ERR:BADCMDPA] ">>, Path, endl, <<"reason: ">>, Err];
log_error(bad_size, {Name, Path}) ->
    [<<"  Incorrect size of the downloaded file/torrent:">>, endl,
     <<"  [ERR:BADSIZEF] ">>, Name, endl,
     <<"  [ERR:BADSIZEP] ">>, Path];
log_error(bad_sum, {Name, Path}) ->
    [<<"  Incorrect SHA1/MD5 sum of the downloaded file/torrent:">>, endl,
     <<"  [ERR:BADSUMFI] ">>, Name, endl,
     <<"  [ERR:BADSUMPA] ">>, Path];
log_error(sum_error, {Name, Path, Type, Err}) ->
    [<<"  Couldn't verify ">>, Type,
     <<" sum for the downloaded file/torrent:">>, endl,
     <<"  [ERR:SUMCMDFI] ">>, Name, endl,
     <<"  [ERR:SUMCMDPA] ">>, Path, endl, <<"reason: ">>, Err];
log_error(rename, {Name, Path, Err}) ->
    [<<"  Couldn't move to the destination folder file/torrent:">>, endl,
     <<"  [ERR:MOVEFILE] ">>, Name, endl,
     <<"  [ERR:MOVEPATH] ">>, Path, endl, <<"reason: ">>, Err];
log_error(stale_err, {Name, Err, DelRes}) ->
    [<<"  Couldn't download file with 'wget':">>, endl,
     <<"  [ERR:WGETFILE] ">>, Name, endl, <<"reason: ">>, Err, endl,
     <<"  result of deleting the file: ">>, DelRes];
log_error(error, {Name, Path, Err}) ->
    [<<"  [ERR:GEN_FILE] ">>, Name, endl,
     <<"  [ERR:GEN_PATH] ">>, Path, endl, <<"reason: ">>, Err].


log_exit(Pid, Reason) ->
    yolog:tin(<<"[ERR:PROCEXIT] Process ">>, Pid,
              <<" terminated with reason ">>, Reason).

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
    ylog:in(<<"Error: The download path '">>, Path, <<"' already exists.">>),
    hbd_event:already_exists(Path),
    throw(already_exists).

ensure_dir(Path, Id) ->
    FileName = filename:join(Path, <<Id/binary, <<".log">>/binary>>),
    case filelib:ensure_dir(FileName) of
        ok -> start_log(FileName);
        {error, _} = Err -> bad_dir(Path, Err)
    end.

bad_dir(Path, Err) ->
    ylog:in(<<"Can't create the download folder '">>, Path, <<"'.">>, endl,
            <<"Error: ">>, Err),
    hbd_event:bad_download_dir(Path, Err),
    throw(Err).

start_log(FileName) ->
    case yolog:init(FileName) of
        ok -> ok;
        Err -> no_log(FileName, Err)
    end.

no_log(FileName, Err) ->
    ylog:in(<<"Can't open log file '">>, FileName, <<"'.">>, endl,
            <<"Error: ">>, Err),
    hbd_event:bad_log_path(FileName, Err),
    throw(Err).

check_done(Type, Pid, #st{pids = Set, count = Count, ends = Ends} = St) ->
    NewSet = sets:del_element(Pid, Set),
    NewEnds = incr_ends(Type, Ends),
    NewSt = St#st{pids = NewSet, ends = NewEnds},
    case sets:size(NewSet) of
        0 -> id_finished(NewSt, Count, NewEnds);
        _ -> {noreply, NewSt}
    end.

incr_ends(done,     {OK, Ign, Exc, Err}) -> {OK + 1, Ign,     Exc,     Err};
incr_ends(ignored,  {OK, Ign, Exc, Err}) -> {OK,     Ign + 1, Exc,     Err};
incr_ends(excluded, {OK, Ign, Exc, Err}) -> {OK,     Ign,     Exc + 1, Err};
incr_ends(error,    {OK, Ign, Exc, Err}) -> {OK,     Ign,     Exc,     Err + 1};
incr_ends(ok,       Ends)                -> Ends.

id_finished(St, Count, {OK, Ign, Exc, Err}) ->
    Status = if Count - OK - Ign - Exc - Err =:= 0 -> ok; true -> error end,
    yolog:tin([<<"Finished all downloads, Expected: ">>, Count,
               <<", Downloaded: ">>, OK,  <<", Ignored: ">>, Ign,
               <<", Excluded: ">>,   Exc, <<", Errors: ">>,  Err,
               <<", Status: ">>, Status, <<", closing log.">>]),
    yolog:stop(),
    {stop, normal, St}.

%%------------------------------------------------------------------------------

do_status(#st{id = Id, url = Url, cookie = Cookie, out = Out, pids = Pids,
              count = Count, ends = Ends}) ->
    [{id, Id}, {url, Url}, {cookie, Cookie}, {out, Out}, {count, Count},
     {ends, Ends}, {pids, sets:to_list(Pids)}].

%%------------------------------------------------------------------------------

start(#st{id = Id, url = Url, cookie = Cookie, out = OutDir} = St) ->
    yolog:tin(<<"Log started.">>),
    Data = hbd_json:process(Url, Id, Cookie, OutDir),
    TorrentDir = filename:join(OutDir, ?TORRENTS_DIR),
    ok = yocmd:mk_dir(TorrentDir),
    [spawn_line(TorrentDir, X, St) || X <- Data].

spawn_line(TrDir, #{folder := F, title := T, downloads := Downloads}, St) ->
    Line = filename:join(F, T),
    LogPid = self(),
    Fun = fun() -> start_line(LogPid, TrDir, Line, F, T, Downloads, St) end,
    Pid = proc_lib:spawn_link(Fun),
    All = lists:sum([length(X) || #{structs := X} <- Downloads]),
    log_process(Pid, Line, All),
    {Pid, All}.

start_line(Parent, TrDir, Line, F, T, Downloads, St) ->
    Self = self(),
    proc_lib:init_ack(Parent, {ok, Self}),
    Items = [merge_one(X, Y, F, T) || #{structs := S} = X <- Downloads, Y <- S],
    Fun = fun(X) -> spawn_one(Parent, TrDir, Line, X, St) end,
    lists:foreach(Fun, Items),
    finish_process(Parent, Self).

merge_one(#{machname := MName, platform := Platform}, Struct, F, T) ->
    Struct#{machname => MName, platform => Platform, folder => F, title => T}.

spawn_one(LogPid, TrDir, Line, #{platform := Platform} = Item, St) ->
    Path = << Line/binary, <<" - ">>/binary, Platform/binary >>,
    LinePid = self(),
    Fun = fun() -> start_one(LogPid, LinePid, TrDir, Path, Item, St) end,
    Pid = proc_lib:spawn_link(Fun),
    ok = download_started(LogPid, Pid, Path),
    unlink(Pid).

start_one(LogPid, Parent, TrDir, Path, Item, St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    record_process(LogPid),
    case Item of
        #{url := undefined, platform := <<"asmjs">>} ->
            process_ignored(LogPid, asmjs, Path);
        #{url := undefined, stream := Stream} when is_binary(Stream) ->
            process_ignored(LogPid, stream, Path);
        #{sha1 := Sha1, md5 := Md5, size := Size} ->
            case hbd_idx:exists(Sha1, Md5, Size) of
                false -> start_one(LogPid, TrDir, Path, Item, St);
                true -> process_ignored(LogPid, in_idx, {Sha1, Md5, Path})
            end;
        _ ->
            exit(bad_json)
    end.

start_one(LogPid, TrDir, Path, Item, St) ->
    exit_if_excluded(LogPid, Path, Item, St),
    Torrent = maps:get(torrent, Item),
    TrCmd = torrent_cmd(TrDir, Torrent, St#st.cookie),
    DRec = #d{logpid = LogPid,
              out    = filename:join(St#st.out, ?DATA_DIR),
              path   = Path,
              url    = maps:get(url, Item),
              sum    = {maps:get(sha1, Item), maps:get(md5, Item)},
              size   = maps:get(size, Item)},
    Status = start_one1(LogPid, St#st.mode, TrCmd, Path, DRec),
    if Status =:= done; Status =:= error -> add_to_idx(St#st.id, Status, Item);
       true -> exit(bad_status) end.

start_one1(LogPid, normal = Mode, TrCmd, Path, DRec) ->
    case ycmd:ensure_dir(filename:join(DRec#d.out, DRec#d.path)) of
        ok -> do_download(LogPid, Mode, TrCmd, DRec);
        {error, Err} -> process_error(LogPid, mkdir, {Path, Err})
    end;
start_one1(LogPid, idx_add = Mode, TrCmd, _Path, DRec) ->
    do_download(LogPid, Mode, TrCmd, DRec).

add_to_idx(Id, Status, #{sha1 := Sha1, md5 := Md5, size := Size} = Item) ->
    Data = maps:without([sha1, md5, size, name, torrent, machname], Item),
    hbd_idx:add(Id, Sha1, Md5, Size, Status, Data).

%%------------------------------------------------------------------------------

exit_if_excluded(LogPid, Path, Item, St) ->
    Vals = [X || X <- maps:values(Item), is_binary(X) orelse yolf:is_string(X)],
    case is_excluded(St#st.regex, Item, Vals) of
        false ->
            ok;
        {true, Captured, Subject} ->
            found_excluded(LogPid, Path, Captured, Subject),
            add_to_idx(St#st.id, excluded, Item),
            exit(normal)
    end.

is_excluded([{name, Re, Opts} | T], Item, Vals) ->
    #{folder := Folder, title := Title, machname := MName, name := Name} = Item,
    Res = check_excluded([Folder, Title, MName, Name], Re, Opts),
    is_excluded(T, Item, Vals, Res);
is_excluded([{link, Re, Opts} | T], Item, Vals) ->
    #{url := Url, torrent := Torrent} = Item,
    Res = check_excluded([Url, Torrent], Re, Opts),
    is_excluded(T, Item, Vals, Res);
is_excluded([{platform, Re, Opts} | T], Item, Vals) ->
    #{platform := Platform} = Item,
    Res = check_excluded([Platform], Re, Opts),
    is_excluded(T, Item, Vals, Res);
is_excluded([{any, Re, Opts} | T], Item, Vals) ->
    Res = check_excluded(Vals, Re, Opts),
    is_excluded(T, Item, Vals, Res);
is_excluded([], _Item, _Vals) ->
    false.

is_excluded(T, Item, Vals, false) -> is_excluded(T, Item, Vals);
is_excluded(_T, _Item, _Vals, Other) -> Other.

check_excluded([Subject|T], Re, Opts) ->
    case re:run(Subject, Re, Opts) of
        nomatch -> check_excluded(T, Re, Opts);
        {match, Captured} -> {true, Captured, Subject}
    end;
check_excluded([], _Re, _Opts) ->
    false.


torrent_cmd(_TrDir, undefined, _Cookie) ->
    undefined;
torrent_cmd(TrDir, Torrent, Cookie) ->
    TorrentFile = filename:join(TrDir, url_to_file(Torrent)),
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -O ">>/binary, TorrentFile/binary,
             <<" \"">>/binary, Torrent/binary, <<"\"">>/binary >>,
    {Cmd, TorrentFile}.

url_to_file(Url) ->
    {ok, {_, _, _, _, Path, _}} = http_uri:parse(binary_to_list(Url)),
    list_to_binary(lists:last(filename:split(Path))).


do_download(LogPid, Mode, {Cmd, TrFile}, DRec) ->
    case yexec:sh_cmd(Cmd) of
        {0, _} ->
            torrent_finished(LogPid, Cmd),
            check_torrent(LogPid, Mode, TrFile, DRec);
        Err ->
            process_warning(LogPid, no_torrent, {Cmd, Err}),
            do_download(LogPid, Mode, undefined, DRec)
    end;
do_download(LogPid, Mode, undefined, #d{url = Url} = DRec) ->
    NewDRec = DRec#d{file = url_to_file(Url)},
    process_download(LogPid, Mode, NewDRec).

check_torrent(LogPid, Mode, TrFile, DRec) ->
    case filelib:is_regular(TrFile) andalso
        etorrent_bcoding:parse_file(TrFile) of
        {ok, BCode} -> process_torrent(LogPid, Mode, TrFile, DRec, BCode);
        false -> bad_torrent(LogPid, Mode, TrFile, DRec, enoent);
        {error, Err} -> bad_torrent(LogPid, Mode, TrFile, DRec, Err)
    end.

bad_torrent(LogPid, Mode, TrFile, DRec, Err) ->
    process_warning(LogPid, bad_torrent, {TrFile, Err}),
    do_download(LogPid, Mode, undefined, DRec).

process_torrent(LogPid, Mode, TrFile, DRec, BCode) ->
    %% Support for torrents with multiple files probably not needed
    [{Name, Size}] = etorrent_io:file_sizes(BCode),
    NewDRec = DRec#d{torrent = TrFile, file = list_to_binary(Name)},
    process_download(LogPid, Mode, Size, NewDRec).

process_download(LogPid, Mode, Size, #d{size = Size} = DRec) ->
    process_download(LogPid, Mode, DRec);
process_download(LogPid, Mode, Size, #d{path = Path, file = File} = DRec) ->
    process_warning(LogPid, torrent_size, {File, Path, Size}),
    do_download(LogPid, Mode, undefined, DRec#d{torrent = undefined}).

process_download(LogPid, normal, #d{path = Path, file = File} = DRec) ->
    case hbd_pool:do_download(DRec) of
        ok ->
            download_finished(LogPid, filename:join(Path, File));
        {error, {duplicate, OtherRec}} ->
            process_error(LogPid, duplicate, {File, Path, OtherRec});
        {error, {file_failed, Err}} ->
            process_error(LogPid, torrent_cmd, {File, Path, Err});
        {error, bad_size} ->
            process_error(LogPid, bad_size, {File, Path});
        {error, bad_sum} ->
            process_error(LogPid, bad_sum, {File, Path});
        {error, {sum_error, Type, Err}} ->
            process_error(LogPid, sum_error, {File, Path, Type, Err});
        {error, {rename, Err}} ->
            process_error(LogPid, rename, {File, Path, Err});
        {error, Err} ->
            process_error(LogPid, error, {File, Path, Err})
    end;
process_download(LogPid, idx_add, #d{path = Path, file = File}) ->
    download_finished(LogPid, filename:join(Path, File)).

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
         torrent_added/3,
         file_started/2,
         stale_detected/2,
         stale_completed/2,
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

-include_lib("yolf/include/yolf.hrl").
-include("download.hrl").

-record(st, {id, url, cookie, regex, out, pids, count, ends}).

%%% API
start_link(Cfg, Id) ->
    ?LOG_WORKER(Id),
    gen_server:start_link(?MODULE, [Cfg, Id], []).

start_download(Pid) ->
    gen_server:cast(Pid, start).

torrent_added(Pid, File, TrFile) ->
    gen_server:cast(Pid, {added, File, TrFile}).

file_started(Pid, File) ->
    gen_server:cast(Pid, {fetching, File}).

stale_detected(Pid, File) ->
    gen_server:cast(Pid, {stale, File}).

stale_completed(Pid, File) ->
    gen_server:cast(Pid, {stale_done, File}).

stale_error(Pid, File, Err, DelRes) ->
    gen_server:cast(Pid, {stale_err, File, Err, DelRes}).

status(Pid) ->
    gen_server:call(Pid, status).

%%------------------------------------------------------------------------------

%%% gen_server callbacks
init([Cfg, Id]) ->
    ?LOG_WORKER_INIT(Id),
    process_flag(trap_exit, true),
    try
        {ok, #st{id     = Id,
                 url    = maps:get(url, Cfg),
                 cookie = maps:get(cookie, Cfg),
                 regex  = maps:get(regex, Cfg),
                 out    = init_out_dir(maps:get(dest, Cfg), Id),
                 pids   = sets:new(),
                 ends   = {0, 0, 0}}}
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
handle_cast({excluded, Pid, Path, Match, Subject}, State) ->
    log_excluded(Pid, Path, Match, Subject),
    check_done(excluded, Pid, State);
handle_cast({torrent, Pid, Cmd, Result}, State) ->
    log_torrent(Pid, Cmd, Result),
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
handle_cast({stale_done, File}, State) ->
    log_stale_done_file(File),
    {noreply, State};
handle_cast({stale_err, File, Err, DelRes}, State) ->
    log_error(stale_err, {File, Err, DelRes}),
    {noreply, State};
handle_cast({done, Pid, Name}, State) ->
    log_downloaded(Pid, Name),
    check_done(done, Pid, State);
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


record_process(LPid, Pid) ->
    gen_server:cast(LPid, {add, Pid}).

finish_process(LPid, Pid) ->
    gen_server:cast(LPid, {remove, Pid}).

found_excluded(LPid, Pid, Path, Match, Subject) ->
    gen_server:cast(LPid, {excluded, Pid, Path, Match, Subject}).

torrent_finished(LPid, Pid, Cmd, Result) ->
    gen_server:cast(LPid, {torrent, Pid, Cmd, Result}).

download_finished(LPid, Pid, Name) ->
    gen_server:cast(LPid, {done, Pid, Name}).

process_error(LPid, Type, Args) ->
    gen_server:cast(LPid, {error, self(), Type, Args}).

%%------------------------------------------------------------------------------

log_process(Pid, Line, Items) ->
    yolog:in([<<"Started pid ">>, Pid, <<" to process ">>, Items,
              <<" download(s) in line ">>, Line, <<".">>]).

log_download(Pid, Line) ->
    yolog:tin(<<"Started pid: ">>, Pid, <<" to download: ">>, endl,
              <<"[NEW_DOWN] ">>, Line).

log_excluded(Pid, Path, Match, Subject) ->
    yolog:tin([<<"Process ">>, Pid, <<" finished, excluded pattern '">>, Match,
               <<"' found in subject '">>, Subject, <<"', download:">>, endl,
               <<"[EXCLDOWN] ">>, Path]).

log_torrent(Pid, Cmd, ok) ->
    yolog:tin(<<"Success, process ">>, Pid,
              <<" downloaded torrent with command: ">>, endl,
              <<"[TOR_DONE] ">>, Cmd);
log_torrent(Pid, Cmd, Result) ->
    yolog:tin([<<"Error: ">>, Result, <<", process ">>, Pid,
               <<" couldn't download torrent with command: ">>, endl,
               <<"[ERR:TOR_DOWN] ">>, Cmd]).

log_torrent_added(Name, TrFile) ->
    yolog:tin([<<"Added to the download queue file/torrent:">>, endl,
               <<"[QADDFILE] ">>, Name, endl,
               <<"[QADD_TOR] ">>, TrFile]).

log_fetching_file(File) ->
    yolog:tin([<<"Started downloading file:">>, endl,
               <<"[FILEDOWN] ">>, File]).

log_stale_file(File) ->
    yolog:tin([<<"Detected stale file, will try to download with 'wget':">>,
               endl, <<"[STALEFIL] ">>, File]).

log_stale_done_file(File) ->
    yolog:tin([<<"Stale file downloaded successfully:">>, endl,
               <<"[STFIDONE] ">>, File]).

log_downloaded(Pid, Name) ->
    yolog:tin(<<"Process ">>, Pid, <<" finished downloading: ">>, endl,
              <<"[FILEDONE] ">>, Name).


log_error(Type, Pid, Args) ->
    yolog:tin([<<"Download error for ">>, Pid, <<": ">>, Type, endl]
              ++ log_error(Type, Args)).

log_error(mkdir, {Path, Err}) ->
    [<<"Couldn't create the destination folder/torrent:">>, endl,
     <<"[ERR:BAD_PATH] ">>, Path, endl, <<"reason: ">>, Err];
log_error(bad_torrent, {Name, Err}) ->
    [<<"[ERR:BAD__TOR] ">>, Name, endl, <<"reason: ">>, Err];
log_error(duplicate, {Name, TrFile}) ->
    [<<"Duplicate file name, can't download file/torrent:">>, endl,
     <<"[ERR:DUPLFILE] ">>, Name, endl,
     <<"[ERR:DUPL_TOR] ">>, TrFile];
log_error(torrent_cmd, {Name, TrFile, Err}) ->
    [<<"Couldn't start bittorrent download:">>, endl,
     <<"[ERR:BADCMDFI] ">>, Name, endl,
     <<"[ERR:BADCMDTR] ">>, TrFile, endl, <<"reason: ">>, Err];
log_error(bad_size, {Name, TrFile}) ->
    [<<"Incorrect size of the downloaded file/torrent:">>, endl,
     <<"[ERR:BADSIZEF] ">>, Name, endl,
     <<"[ERR:BADSIZET] ">>, TrFile];
log_error(bad_sum, {Name, TrFile, Type}) ->
    [<<"Incorrect ">>, Type, <<" sum of the downloaded file/torrent:">>, endl,
     <<"[ERR:BADSUMFI] ">>, Name, endl,
     <<"[ERR:BADSUMTR] ">>, TrFile];
log_error(sum_error, {Name, TrFile, Type, Err}) ->
    [<<"Couldn't verify ">>, Type,
     <<" sum for the downloaded file/torrent:">>, endl,
     <<"[ERR:SUMCMDFI] ">>, Name, endl,
     <<"[ERR:SUMCMDTR] ">>, TrFile, endl, <<"reason: ">>, Err];
log_error(rename, {Name, TrFile, Err}) ->
    [<<"Couldn't move to the destination folder file/torrent:">>, endl,
     <<"[ERR:MOVEFILE] ">>, Name, endl,
     <<"[ERR:MOVE_TOR] ">>, TrFile, endl, <<"reason: ">>, Err];
log_error(stale_err, {Name, Err, DelRes}) ->
    [<<"Couldn't download file with 'wget':">>, endl,
     <<"[ERR:WGETFILE] ">>, Name, endl, <<"reason: ">>, Err, endl,
     <<"result of deleting the file: ">>, DelRes];
log_error(error, {Name, TrFile, Err}) ->
    [<<"[ERR:GEN_FILE] ">>, Name, endl,
     <<"[ERR:GEN__TOR] ">>, TrFile, endl, <<"reason: ">>, Err].

log_exit(Pid, Reason) ->
    yolog:tin(<<"Process ">>, Pid, <<" terminated with reason ">>, Reason).

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

check_done(Type, Pid, #st{pids = Set, count = Count, ends = Ends} = St) ->
    NewSet = sets:del_element(Pid, Set),
    NewEnds = incr_ends(Type, Ends),
    NewSt = St#st{pids = NewSet, ends = NewEnds},
    case sets:size(NewSet) of
        0 -> id_finished(NewSt, Count, NewEnds);
        _ -> {noreply, NewSt}
    end.

incr_ends(error,    {OK, Err, Exc}) -> {OK,     Err + 1, Exc};
incr_ends(excluded, {OK, Err, Exc}) -> {OK,     Err,     Exc + 1};
incr_ends(done,     {OK, Err, Exc}) -> {OK + 1, Err,     Exc};
incr_ends(ok,       Ends)            -> Ends.

id_finished(St, Count, {OK, Err, Exc}) ->
    Status = if Count - OK - Err - Exc =:= 0 -> ok; true -> error end,
    yolog:tin([<<"Finished all downloads, Expected: ">>, Count,
               <<", Downloaded: ">>, OK, <<", Errors: ">>, Err,
               <<", Excluded: ">>, Exc, <<", Status: ">>, Status,
               <<", closing log.">>]),
    yolog:stop(),
    {stop, normal, St}.

%%------------------------------------------------------------------------------

do_status(#st{id = Id, url = Url, cookie = Cookie, out = Out, pids = Pids,
              count = Count, ends = Ends}) ->
    [{id, Id}, {url, Url}, {cookie, Cookie}, {out, Out}, {count, Count},
     {ends, Ends}, {pids, sets:to_list(Pids)}].

%%------------------------------------------------------------------------------

start(#st{id = Id, url = Url, cookie = Cookie, out = OutDir} = St) ->
    Data = hbd_json:process(Url, Id, Cookie, OutDir),
    TorrentDir = filename:join(OutDir, <<"torrents">>),
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

spawn_one(LogPid, TrDir, Line, Item, St) ->
    Path = filename:join(Line, get_download_name(Item)),
    LinePid = self(),
    Fun = fun() -> start_one(LogPid, LinePid, TrDir, Path, Item, St) end,
    Pid = proc_lib:spawn_link(Fun),
    ok = download_started(LogPid, Pid, Path),
    unlink(Pid).

get_download_name(#{platform := Platform, name := Name, machname := MName}) ->
    Sep = <<" - ">>,
    << Platform/binary, Sep/binary, Name/binary, Sep/binary, MName/binary >>.

start_one(LogPid, Parent, TrDir, Path, Item, St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    record_process(LogPid, self()),
    exit_if_excluded(LogPid, Path, St#st.regex, Item),
    Torrent = maps:get(torrent, Item),
    TrCmd = torrent_cmd(TrDir, Torrent, St#st.cookie),
    DRec = #d{logpid = LogPid,
              out    = St#st.out,
              path   = Path,
              url    = maps:get(url, Item),
              sum    = get_sum(Item),
              size   = maps:get(size, Item)},
    case ycmd:ensure_dir(filename:join(DRec#d.out, Path)) of
        ok -> start_torrent(LogPid, TrCmd, DRec);
        {error, Err} -> process_error(LogPid, mkdir, {Path, Err})
    end,
    exit(normal).

exit_if_excluded(LogPid, Path, List, Item) ->
    Vals = [X || X <- maps:values(Item), is_binary(X) orelse yolf:is_string(X)],
    exit_if_excluded(LogPid, Path, List, Item, Vals).

exit_if_excluded(LogPid, Path, [{name, Re, Opts} | T], Item, Vals) ->
    #{folder := Folder, title := Title, machname := MName, name := Name} = Item,
    check_excluded(LogPid, Path, Folder, Re, Opts),
    check_excluded(LogPid, Path, Title,  Re, Opts),
    check_excluded(LogPid, Path, MName,  Re, Opts),
    check_excluded(LogPid, Path, Name,   Re, Opts),
    exit_if_excluded(LogPid, Path, T, Item, Vals);
exit_if_excluded(LogPid, Path, [{link, Re, Opts} | T], Item, Vals) ->
    #{url := Url, torrent := Torrent} = Item,
    check_excluded(LogPid, Path, Url,     Re, Opts),
    check_excluded(LogPid, Path, Torrent, Re, Opts),
    exit_if_excluded(LogPid, Path, T, Item, Vals);
exit_if_excluded(LogPid, Path, [{platform, Re, Opts} | T], Item, Vals) ->
    #{platform := Platform} = Item,
    check_excluded(LogPid, Path, Platform, Re, Opts),
    exit_if_excluded(LogPid, Path, T, Item, Vals);
exit_if_excluded(LogPid, Path, [{any, Re, Opts} | T], Item, Vals) ->
    Fun = fun(X) -> check_excluded(LogPid, Path, X, Re, Opts) end,
    lists:foreach(Fun, Vals),
    exit_if_excluded(LogPid, Path, T, Item, Vals);
exit_if_excluded(_LogPid, _Path, [], _Item, _Vals) ->
    ok.

check_excluded(LogPid, Path, Subject, Re, Opts) ->
    check_excluded(LogPid, Path, Subject, re:run(Subject, Re, Opts)).

check_excluded(_LogPid, _Path, _Subject, nomatch) ->
    ok;
check_excluded(LogPid, Path, Subject, {match, Captured}) ->
    found_excluded(LogPid, self(), Path, Captured, Subject),
    exit(normal).

torrent_cmd(TrDir, Torrent, Cookie) ->
    {ok, {_, _, _, _, Path, _}} = http_uri:parse(binary_to_list(Torrent)),
    File = lists:last(filename:split(Path)),
    TorrentFile = filename:join(TrDir, File),
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -O ">>/binary, TorrentFile/binary,
             <<" \"">>/binary, Torrent/binary, <<"\"">>/binary >>,
    {Cmd, TorrentFile}.

get_sum(#{sha1 := undefined, md5 := Md5}) -> {md5, Md5};
get_sum(#{sha1 := Sha1}) when Sha1 =/= undefined -> {sha1, Sha1}.

start_torrent(LogPid, {Cmd, TrFile}, DRec) ->
    case yexec:sh_cmd(Cmd) of
        {0, _} ->
            torrent_finished(LogPid, self(), Cmd, ok),
            check_torrent(LogPid, TrFile, DRec);
        Err ->
            torrent_finished(LogPid, self(), Cmd, Err),
            exit(no_torrent)
    end.

check_torrent(LogPid, TrFile, DRec) ->
    case filelib:is_regular(TrFile) andalso
        etorrent_bcoding:parse_file(TrFile) of
        {ok, BCode} -> process_torrent(LogPid, TrFile, DRec, BCode);
        false -> bad_torrent(LogPid, TrFile, enoent);
        {error, Err} -> bad_torrent(LogPid, TrFile, Err)
    end.

bad_torrent(LogPid, TrFile, Err) ->
    process_error(LogPid, bad_torrent, {TrFile, Err}).

process_torrent(LogPid, TrFile, DRec, BCode) ->
    %% Support for torrents with multiple files probably not needed
    [{Name, Size}] = etorrent_io:file_sizes(BCode),
    Size = DRec#d.size,
    NewDRec = DRec#d{torrent = TrFile, file = list_to_binary(Name)},
    case hbd_pool:do_torrent(NewDRec) of
        ok ->
            download_finished(LogPid, self(), filename:join(DRec#d.path, Name));
        {error, duplicate} ->
            process_error(LogPid, duplicate, {Name, TrFile});
        {error, {file_failed, Err}} ->
            process_error(LogPid, torrent_cmd, {Name, TrFile, Err});
        {error, bad_size} ->
            process_error(LogPid, bad_size, {Name, TrFile});
        {error, {bad_sum, Type}} ->
            process_error(LogPid, bad_sum, {Name, TrFile, Type});
        {error, {sum_error, Type, Err}} ->
            process_error(LogPid, sum_error, {Name, TrFile, Type, Err});
        {error, {rename, Err}} ->
            process_error(LogPid, rename, {Name, TrFile, Err});
        {error, Err} ->
            process_error(LogPid, error, {Name, TrFile, Err})
    end.

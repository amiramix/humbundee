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
-include("download.hrl").

-record(st, {id, url, cookie, regex, in, out, pids, ends}).

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
        {ok, #st{id     = Id,
                 url    = maps:get(url, Cfg),
                 cookie = maps:get(cookie, Cfg),
                 regex  = maps:get(regex, Cfg),
                 in     = maps:get(in, Cfg),
                 out    = init_out_dir(maps:get(out, Cfg), Id),
                 pids   = sets:new(),
                 ends   = {0, 0}}}
    catch
        throw:Term -> {stop, Term}
    end.

handle_call({started, Pid, Name}, _From, #st{pids = Set} = St) ->
    link(Pid),
    log_download(Pid, Name),
    {reply, ok, St#st{pids = sets:add_element(Pid, Set)}};
handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast(start, #st{pids = Set} = St) ->
    Pids = start(St),
    {noreply, St#st{pids = sets:union(Set, sets:from_list(Pids))}};
handle_cast({torrent, Pid, Cmd, Result}, State) ->
    log_torrent(Pid, Cmd, Result),
    {noreply, State};
handle_cast({added, FileName}, State) ->
    log_torrent_added(FileName),
    {noreply, State};
handle_cast({file, Pid, Name}, State) ->
    log_downloaded(Pid, Name),
    check_done(ok, Pid, State);
handle_cast({bad_torrent, Pid, Name}, State) ->
    log_bad_torrent(Pid, Name),
    check_done(error, Pid, State);
handle_cast({excluded, Pid, Match, Subject}, State) ->
    log_excluded(Pid, Match, Subject),
    check_done(excluded, Pid, State);
handle_cast(_, State) ->
    {noreply, State}.

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
download_started(Pid, NewPid, Name) ->
    gen_server:call(Pid, {started, NewPid, Name}).

torrent_finished(Pid, NewPid, Cmd, Result) ->
    gen_server:cast(Pid, {torrent, NewPid, Cmd, Result}).

download_finished(Pid, NewPid, Name) ->
    gen_server:cast(Pid, {file, NewPid, Name}).

bad_torrent(Pid, NewPid, Name) ->
    gen_server:cast(Pid, {bad_torrent, NewPid, Name}).

found_excluded(Pid, NewPid, Match, Subject) ->
    gen_server:cast(Pid, {excluded, NewPid, Match, Subject}).

%%------------------------------------------------------------------------------

log_download(Pid, Line) ->
    yolog:tin(<<"Started pid: ">>, Pid, <<" to download: ">>, Line).

log_torrent(Pid, Cmd, ok) ->
    yolog:tin(<<"Success, process ">>, Pid,
              <<" downloaded torrent with command: ">>, endl, Cmd);
log_torrent(Pid, Cmd, Result) ->
    yolog:tin(<<"Error: ">>, Result, <<", process ">>, Pid,
              <<" couldn't download torrent with command: ">>, endl, Cmd).

log_torrent_added(FileName) ->
    yolog:tin(<<"Torrent '">>, FileName, <<"' added to the download queue">>).

log_downloaded(Pid, Name) ->
    yolog:tin(<<"Process ">>, Pid, <<" finished downloading: ">>, Name).

log_bad_torrent(Pid, Name) ->
    yolog:tin(<<"Error, process ">>, Pid,
              <<" can't find downloaded torrent ">>, Name).

log_excluded(Pid, Match, Subject) ->
    yolog:tin(<<"Process ">>, Pid, <<" finished, exluded pattern '">>, Match,
              <<"' found in subject '">>, Subject, <<"'.">>).

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

check_done(Type, Pid, #st{pids = Set, ends = Ends} = St) ->
    NewSet = sets:del_element(Pid, Set),
    NewEnds = incr_ends(Type, Ends),
    NewSt = St#st{pids = NewSet, ends = NewEnds},
    case sets:size(NewSet) of
        0 -> id_finished(NewSt, NewEnds);
        _ -> {noreply, NewSt}
    end.

incr_ends(error,    {Err, Exc}) -> {Err + 1, Exc};
incr_ends(excluded, {Err, Exc}) -> {Err, Exc + 1};
incr_ends(ok,       Ends)       -> Ends.

id_finished(St, {Err, Exc}) ->
    yolog:tin(<<"Finished all downloads, Errors: ">>, Err,
              <<", Excluded: ">>, Exc, <<", closing log.">>),
    yolog:stop(),
    {stop, normal, St}.

%%------------------------------------------------------------------------------

start(#st{id = Id, url = Url, cookie = Cookie, out = OutDir} = St) ->
    Data = hbd_json:process(Url, Id, Cookie, OutDir),
    TorrentDir = filename:join(OutDir, <<"torrents">>),
    ok = yocmd:mk_dir(TorrentDir),
    [spawn_line(TorrentDir, X, St) || X <- Data].

spawn_line(TrDir, #{folder := F, title := T, downloads := Downloads}, St) ->
    Line = filename:join(F, T),
    Fun = fun() -> start_line(self(), TrDir, Line, F, T, Downloads, St) end,
    Pid = proc_lib:spawn_link(Fun),
    log_download(Pid, Line),
    Pid.

start_line(Parent, TrDir, Line, F, T, Downloads, St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    Items = [merge_one(X, Y, F, T) || #{structs := S} = X <- Downloads, Y <- S],
    Fun = fun(X) -> spawn_one(Parent, TrDir, Line, X, St) end,
    lists:foreach(Fun, Items).

merge_one(#{machname := MName, platform := Platform}, Struct, F, T) ->
    Struct#{machname => MName, platform => Platform, folder => F, title => T}.

spawn_one(LogPid, TrDir, Line, Item, St) ->
    Path = filename:join(Line, get_download_name(Item)),
    Fun = fun() -> start_one(self(), LogPid, TrDir, Path, Item, St) end,
    Pid = proc_lib:spawn_link(Fun),
    ok = download_started(LogPid, Pid, Path),
    unlink(Pid).

get_download_name(#{platform := Platform, name := Name, machname := MName}) ->
    Sep = <<" - ">>,
    << Platform/binary, Sep/binary, Name/binary, Sep/binary, MName/binary >>.

start_one(Parent, LogPid, TrDir, Path, Item, St) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    exit_if_excluded(LogPid, St#st.regex, Item),
    Torrent = maps:get(torrent, Item),
    TrCmd = torrent_cmd(TrDir, Torrent, St#st.cookie),
    DRec = #d{logpid  = LogPid,
              torrent = Torrent,
              in      = St#st.in,
              out     = St#st.out,
              path    = Path,
              sum     = get_sum(Item),
              size    = maps:get(size, Item)},
    start_torrent(LogPid, TrCmd, DRec).

exit_if_excluded(LogPid, List, Item) ->
    Vals = [X || X <- maps:values(Item), is_binary(X) orelse yolf:is_string(X)],
    exit_if_excluded(LogPid, List, Item, Vals).

exit_if_excluded(LogPid, [{name, Re, Opts} | T], Item, Vals) ->
    #{folder := Folder, title := Title, machname := MName, name := Name} = Item,
    check_excluded(LogPid, Folder, Re, Opts),
    check_excluded(LogPid, Title,  Re, Opts),
    check_excluded(LogPid, MName,  Re, Opts),
    check_excluded(LogPid, Name,   Re, Opts),
    exit_if_excluded(LogPid, T, Item, Vals);
exit_if_excluded(LogPid, [{link, Re, Opts} | T], Item, Vals) ->
    #{url := Url, torrent := Torrent} = Item,
    check_excluded(LogPid, Url,     Re, Opts),
    check_excluded(LogPid, Torrent, Re, Opts),
    exit_if_excluded(LogPid, T, Item, Vals);
exit_if_excluded(LogPid, [{platform, Re, Opts} | T], Item, Vals) ->
    #{platform := Platform} = Item,
    check_excluded(LogPid, Platform, Re, Opts),
    exit_if_excluded(LogPid, T, Item, Vals);
exit_if_excluded(LogPid, [{any, Re, Opts} | T], Item, Vals) ->
    lists:foreach(fun(X) -> check_excluded(LogPid, X, Re, Opts) end, Vals),
    exit_if_excluded(LogPid, T, Item, Vals);
exit_if_excluded(_LogPid, [], _Item, _Vals) ->
    ok.

check_excluded(LogPid, Subject, Re, Opts) ->
    check_excluded(LogPid, Subject, re:run(Subject, Re, Opts)).

check_excluded(_LogPid, _Subject, nomatch) ->
    ok;
check_excluded(LogPid, Subject, {match, Captured}) ->
    found_excluded(LogPid, self(), Captured, Subject),
    exit(normal).

torrent_cmd(TrDir, Torrent, Cookie) ->
    {ok, {_, _, _, _, Path, _}} = http_uri:parse(binary_to_list(Torrent)),
    File = lists:last(filename:split(Path)),
    TorrentFile = filename:join(TrDir, File),
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -P ">>/binary, TrDir/binary, <<" ">>/binary, Torrent/binary >>,
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
    case filelib:is_regular(TrFile) of
        true -> process_torrent(LogPid, TrFile, DRec);
        false -> bad_torrent(LogPid, self(), TrFile)
    end.

process_torrent(LogPid, TrFile, DRec) ->
    hbd_pool:do_torrent(TrFile, DRec),
    download_finished(LogPid, self(), DRec#d.path).

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

-module(hbd_pool).
-behaviour(gen_server).

%% API
-export([
         start_link/1,
         do_download/1,
         status/0
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

-include_lib("kernel/include/file.hrl").
-include_lib("yolf/include/yolf.hrl").
-include("download.hrl").

-define(INTERVAL, 2000).
-define(TORRENT_TIMEOUT, 300).
-record(st, {max, cookie, in, tmp, ids, bad, q, pids}).

%%% API
start_link(Cfg) ->
    ?LOG_WORKER(?MODULE),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Cfg], []).

do_download(DRec) -> gen_server:call(?MODULE, {do, DRec}, infinity).

status() -> gen_server:call(?MODULE, status).

%%% gen_server callbacks
init([#{workers := Workers, cookie := Cookie, in := In, tmp := Tmp}]) ->
    ?LOG_WORKER_INIT(?MODULE),
    process_flag(trap_exit, true),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #st{max = Workers, cookie = Cookie, in = In, tmp = Tmp, ids = #{},
             bad = sets:new(), q = queue:new(), pids = sets:new()}}.

handle_call({do, #d{file = File} = DRec}, From, #st{q = Q} = St) ->
    {ok, MP} = re:compile(<< <<"^">>/binary, File/binary >>),
    Download = DRec#d{local = #l{from = From, rex = MP}},
    NewQ = queue:in(Download, Q),
    hbd_id:download_added(DRec#d.logpid, File, DRec#d.torrent),
    {noreply, start_downloads(St#st{q = NewQ})};
handle_call(status, _From,
            St = #st{ids = Ids, bad = Bad, q = Q, pids = Pids}) ->
    Ret = [{downloading, maps:keys(Ids)}, {bad, sets:to_list(Bad)},
           {waiting, queue:len(Q)}, {wget, sets:to_list(Pids)}],
    {reply, Ret, St};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Pid}, #st{pids = Pids} = St) ->
    {noreply, St#st{pids = sets:add_element(Pid, Pids)}};
handle_cast({remove, Pid}, #st{pids = Pids} = St) ->
    {noreply, St#st{pids = sets:del_element(Pid, Pids)}};
handle_cast({stale_err, Pid, File},
            #st{ids = Ids, bad = Bad, pids = Pids} = St) ->
    NewSt = St#st{ids = maps:remove(File, Ids),
                  bad = sets:add_element(File, Bad),
                  pids = sets:del_element(Pid, Pids)},
    {noreply, NewSt};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(trigger, St) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, check_stale(start_downloads(check_completed(St)))};
handle_info({'EXIT', Pid, Reason}, #st{pids = Pids} = St) ->
    ylog:tin(<<"Process ">>, Pid, <<" terminated with reason ">>, Reason),
    {noreply, St#st{pids = sets:del_element(Pid, Pids)}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%%% Internal methods

check_completed(#st{in = In, ids = Ids, bad = OrgBad} = St) ->
    All = ycmd:ls_dir(In),
    Bad = sets:filter(fun(X) -> lists:member(X, All) end, OrgBad),
    Paths = [{X, filename:join(In, X)} || X <- All],
    Files = [T || {X, Path} = T <- Paths,
                  not sets:is_element(X, Bad), filelib:is_regular(Path)],
    {NIds, NBad} = lists:foldl(fun move_file/2, {Ids, Bad}, Files),
    St#st{ids = NIds, bad = NBad}.

move_file({File, Path}, {Ids, Bad}) ->
    case maps:find(File, Ids) of
        {ok, #d{local = Loc} = DRec} ->
            Res = process_file(File, Path, DRec),
            gen_server:reply(Loc#l.from, Res),
            {maps:remove(File, Ids), update_bad(Res, File, Bad)};
        error ->
            ylog:tin(<<"Error: Unrecognized file in the download folder '">>,
                     File, <<"'.">>),
            {Ids, sets:add_element(File, Bad)}
    end.

update_bad(ok, _File, Bad) -> Bad;
update_bad({error, _}, File, Bad) -> sets:add_element(File, Bad).

process_file(File, Path, #d{size = Size} = DRec) ->
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} -> check_sum(File, Path, DRec);
        {ok, _} -> {error, bad_size};
        {error, _} = Err -> Err
    end.

check_sum(File, SrcPath, #d{sum = Sum, out = Out, path = DestDir} = DRec) ->
    case check_sum(SrcPath, Sum, DRec#d.logpid, File) of
        ok -> mv_file(File, SrcPath, filename:join(Out, DestDir));
        {error, _} = Err -> Err
    end.

check_sum(Path, {undefined, Md5}, LogPid, File) ->
    check_md5(Path, Md5, LogPid, File);
check_sum(Path, {Sha1, Md5}, LogPid, File) ->
    case yexec:sh_cmd(<< <<"sha1 -q ">>/binary, Path/binary >>) of
        {0, [Sha1]} -> ok;
        {0, _} -> check_md5(Path, Md5, LogPid, File);
        Err -> {error, {sum_error, sha1, Err}}
    end.

check_md5(Path, Md5, LogPid, File) ->
    hbd_id:warn_bad_sha1(LogPid, File),
    case yexec:sh_cmd(<< <<"md5 -q ">>/binary, Path/binary >>) of
        {0, [Md5]} -> ok;
        {0, _} -> {error, bad_sum};
        Err -> {error, {sum_error, md5, Err}}
    end.

mv_file(File, Path, Dest) ->
    DestFile = filename:join(Dest, File),
    case filelib:ensure_dir(DestFile) of
        ok ->
            case file:rename(Path, DestFile) of
                ok -> ok;
                {error, Err} -> {error, {rename, Err}}
            end;
        {error, Err} ->
            {error, {mkdir, Err}}
    end.

%%------------------------------------------------------------------------------

start_downloads(#st{max = Max, ids = Ids} = St) ->
    case Max - maps:size(Ids) of
        0 -> St;
        X when X > 0 -> fill_downloads(X, St)
    end.

fill_downloads(X, #st{ids = Ids, q = Q} = St) ->
    {NewIds, NewQ} = fill_downloads(X, Ids, Q, St),
    St#st{ids = NewIds, q = NewQ}.

fill_downloads(0, Ids, Q, _St) -> {Ids, Q};
fill_downloads(X, Ids, Q, St)  -> next_download(X, Ids, queue:out(Q), St).

next_download(_X, Ids, {empty, Q}, _St) ->
    {Ids, Q};
next_download(X, Ids, {{value, #d{file = File, local = Loc} = DRec}, Q}, St) ->
    case maps:is_key(File, Ids) of
        true ->
            check_same(DRec, maps:get(File, Ids)),
            fill_downloads(X, Ids, Q, St);
        false ->
            {Sha1, Md5} = DRec#d.sum,
            case hbd_idx:exists(Sha1, Md5, DRec#d.size) of
                false ->
                    next_download(X, Ids, DRec, Q, St);
                true ->
                    gen_server:reply(Loc#l.from, {in_idx, Sha1, Md5}),
                    fill_downloads(X, Ids, Q, St)
            end
    end.

check_same(#d{sum = {Sha1, Md5}, size = S, local = #l{from = From}},
           #d{sum = {Sha1, Md5}, size = S} = Other) ->
    gen_server:reply(From, {duplicate, other_info(Other)});
check_same(#d{local = #l{from = From}}, Other) ->
    gen_server:reply(From, {error, {duplicate, other_info(Other)}}).

other_info(#d{out = O, path = P, file = F, sum = {Sha1, Md5}, size = S}) ->
    [{path, filename:join([O, P, F])}, {sha1, Sha1}, {md5, Md5}, {size, S}].

next_download(X, Ids, #d{torrent = undefined, local = Loc} = DRec, Q, St) ->
    #d{logpid = LogPid, file = File} = DRec,
    hbd_id:file_started(LogPid, File),
    download_file(DRec, St),
    NLoc = Loc#l{ts = erlang:monotonic_time(seconds)},
    fill_downloads(X - 1, Ids#{File => DRec#d{local = NLoc}}, Q, St);
next_download(X, Ids, #d{local = Loc} = DRec, Q, St) ->
    #d{logpid = LogPid, torrent = TrFile, file = File} = DRec,
    case yexec:sh_cmd(<< <<"qbittorrent-nox ">>/binary, TrFile/binary >>) of
        {0, _} ->
            hbd_id:file_started(LogPid, File),
            NLoc = Loc#l{ts = erlang:monotonic_time(seconds)},
            fill_downloads(X - 1, Ids#{File => DRec#d{local = NLoc}}, Q, St);
        Err ->
            gen_server:reply(Loc#l.from, {error, {torrent_cmd, Err}}),
            fill_downloads(X, Ids, Q, St)
    end.

%%------------------------------------------------------------------------------

check_stale(#st{tmp = Tmp, ids = Ids} = St) ->
    Ts = erlang:monotonic_time(seconds) - ?TORRENT_TIMEOUT,
    Old = [Y || Y = #d{local = Loc} <- maps:values(Ids),
                Loc#l.ts < Ts, Loc#l.skip =/= true],
    case length(Old) > 0 andalso ycmd:ls_dir(Tmp) of
        false -> St;
        [] -> process_stale(Old, St#st.ids, St);
        L when is_list(L) -> detect_stale(Old, L, St#st.ids, St)
    end.

detect_stale(Old, Files, Ids, St) ->
    Fun = fun(#d{local = #l{rex = Rex}}) ->
                  lists:all(fun(X) -> re:run(X, Rex) =:= nomatch end, Files)
          end,
    process_stale(lists:filter(Fun, Old), Ids, St).

process_stale([#d{file = File, local = Loc} = DRec|T], Ids, St) ->
    hbd_id:stale_detected(DRec#d.logpid, File),
    download_file(DRec, St),
    NIds = Ids#{File := DRec#d{local = Loc#l{skip = true}}},
    process_stale(T, NIds, St);
process_stale([], Ids, St) ->
    St#st{ids = Ids}.

%%------------------------------------------------------------------------------

download_file(DRec, #st{cookie = Cookie, in = In, tmp = Tmp}) ->
    Self = self(),
    Fun = fun() -> download_file(Self, Cookie, In, Tmp, DRec) end,
    proc_lib:spawn_link(Fun).

download_file(Parent, Cookie, In, Tmp, #d{local = #l{from = From}} = DRec) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    gen_server:cast(?MODULE, {add, self()}),
    #d{logpid = LogPid, url = Url, file = File} = DRec,
    Path = << (filename:join(Tmp, File))/binary, <<".hbd!">>/binary >>,
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -O ">>/binary, Path/binary, <<" \"">>/binary, Url/binary,
             <<"\"">>/binary >>,
    case yexec:sh_cmd(Cmd) of
        {0, _} ->
            case file:rename(Path, filename:join(In, File)) of
                ok ->
                    hbd_id:download_completed(LogPid, File),
                    gen_server:cast(?MODULE, {remove, self()});
                {error, Err} ->
                    mark_failed(From, File, Path, Err)
            end;
        Err ->
            mark_failed(From, File, Path, Err)
    end.

mark_failed(From, File, Path, Err) ->
    Res = file:delete(Path),
    gen_server:reply(From, {error, {stale_err, Err, Res}}),
    gen_server:cast(?MODULE, {stale_err, self(), File}).

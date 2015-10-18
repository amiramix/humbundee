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

-module(hbd_api).
-behaviour(gen_server).

%% API
-export([start_link/1,
         download/1]).

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

-record(st, {cfg, ids, pids}).

%%% API
start_link(Cfg) ->
    ?LOG_WORKER(?MODULE),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Cfg], []).

download(Id) when is_binary(Id) ->
    gen_server:cast(?MODULE, {download, Id});
download(_) ->
    yio:en(<<"Please specify Id as binary">>).

%%% gen_server callbacks
init([Cfg]) ->
    ?LOG_WORKER_INIT(?MODULE),
    process_flag(trap_exit, true),
    {ok, #st{cfg = Cfg, ids = #{}, pids = #{}}}.

handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast({download, Id}, State) ->
    {noreply, download_id(Id, State)};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    stop_download(Pid, Reason, State);
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal methods

download_id(Id, #st{ids = Ids} = State) ->
    case maps:is_key(Id, Ids) of
        true ->
            yio:en(<<"Id '">>, Id, <<"' already downloading. Ignoring...">>),
            State;
        false ->
            start_download(Id, State)
    end.

start_download(Id, #st{cfg = Cfg, ids = Ids, pids = Pids} = State) ->
    case hbd_id:start_link(Cfg, Id) of
        {ok, Pid} ->
            yio:in(<<"Started downloading for ID: ">>, Id),
            ylog:tin(<<"Download started for ID: ">>, Id),
            hbd_id:start_download(Pid),
            State#st{ids = Ids#{Id => Pid}, pids = Pids#{Pid => Id}};
        {error, _} = Err ->
            yio:en(<<"Can't start the download, error: ">>, Err, endl),
            State
    end.

stop_download(Pid, Reason, #st{pids = Pids} = State) ->
    case maps:is_key(Pid, Pids) of
        false -> no_download(Pid, Reason, State);
        true -> end_download(Pid, Reason, State)
    end.

no_download(Pid, Reason, State) ->
    ylog:tin(<<"Process '">>, Pid, <<"' ended with reason: ">>, Reason),
    {noreply, State}.

end_download(Pid, Reason, #st{ids = Ids, pids = Pids} = State) ->
    Id  = maps:get(Pid, Pids),
    Pid = maps:get(Id,  Ids),
    NewPids = maps:remove(Pid, Pids),
    NewIds  = maps:remove(Id,  Ids),
    ylog:tin(<<"Download ended for ID: ">>, Id, <<", with reason: ">>, Reason),
    {noreply, State#st{ids = NewIds, pids = NewPids}}.

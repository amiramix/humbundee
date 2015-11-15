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

-module(hbd_pool).
-behaviour(gen_server).

%% API
-export([
         start_link/1,
         do_torrent/2
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

-define(INTERVAL, 1000).
-record(st, {max, ids, q}).

%%% API
start_link(Workers) ->
    ?LOG_WORKER(?MODULE),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Workers], []).

do_torrent(TrFile, DRec) ->
    gen_server:call({do, TrFile, DRec}).

%%% gen_server callbacks
init([Workers]) ->
    ?LOG_WORKER_INIT(?MODULE),
    erlang:send_after(?INTERVAL, self(), trigger),
    {ok, #st{max = Workers, ids = #{}, q = queue:new()}}.

handle_call({do, TrFile, DRec}, From, #st{q = Q} = St) ->
    Download = DRec#d{from = From},
    NewQ = queue:in(Download, Q),
    hbd_id:torrent_added(DRec#d.logpid, TrFile),
    {noreply, St#st{q = NewQ}};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(trigger, #st{q = Q} = St) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, St#st{q = next_d(queue:out(Q))}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal methods

next_d({empty, Q}) ->
    Q;
next_d({{value, #d{from = From, path = Path}}, Q}) ->
    ylog:tin(<<"Completing: ">>, Path),
    gen_server:reply(From, ok),
    Q.

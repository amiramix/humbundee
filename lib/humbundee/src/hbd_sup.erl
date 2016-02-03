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

-module(hbd_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("yolf/include/yolf.hrl").

-define(SERVER, ?MODULE).
-define(WORKER2(I, Arg), {I, {I, start_link, [Arg]}, permanent, ?SHUTDOWN_TIMEOUT, worker, [I]}).

start_link(Cfg) ->
    ?LOG_SUPERVISOR(?SERVER),
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Cfg]).

init([Cfg]) ->
    ?LOG_SUPERVISOR_INIT(?SERVER),

    EventMngr = ?WORKER(hbd_event),
    IdxMngr   = ?WORKER(hbd_idx),
    PoolMngr  = ?WORKER2(hbd_pool, Cfg),
    ApiMngr   = ?WORKER2(hbd_api, Cfg),

    Children = [EventMngr, IdxMngr, PoolMngr, ApiMngr],

    {ok, {{one_for_one, 2, 10}, Children}}.

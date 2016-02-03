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

-module(hbd_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2]).

-export([already_exists/1,
         bad_download_dir/2,
         bad_log_path/2]).

-include_lib("yolf/include/yolf.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    ?LOG_WORKER(?SERVER),
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    ok = gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    ok = gen_event:delete_handler(?SERVER, Handler, Args).

%%------------------------------------------------------------------------------

already_exists(Path) ->
    gen_event:notify(?SERVER, {already_exists, Path}).

bad_download_dir(Path, Err) ->
    gen_event:notify(?SERVER, {bad_download_dir, Path, Err}).

bad_log_path(Path, Err) ->
    gen_event:notify(?SERVER, {bad_log_path, Path, Err}).

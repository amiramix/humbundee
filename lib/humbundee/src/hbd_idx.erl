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

-module(hbd_idx).
-behaviour(gen_server).

%% API
-export([
         start_link/0,
         add/5,
         exists/2,
         read/1
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
-include_lib("humbundee/include/humbundee.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    ?LOG_WORKER(?SERVER),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Id, Sha1, Md5, Status, Data) ->
    gen_server:cast(?SERVER, {add, Id, Sha1, Md5, Status, Data}).

exists(Sha1, Md5) ->
    gen_server:call(?SERVER, {exists, Sha1, Md5}).

read(Sum) ->
    gen_server:call(?SERVER, {read, Sum}).

%%------------------------------------------------------------------------------
%%% gen_server callbacks

init([]) ->
    ?LOG_WORKER_INIT(?SERVER),
    {ok, []}.

handle_call({exists, Sha1, Md5}, _From, State) ->
    {reply, check(Sha1, Md5), State};
handle_call({read, Sum}, _From, State) ->
    {reply, do_read(Sum), State};
handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast({add, Id, Sha1, Md5, Status, Data}, State) ->
    store(Id, Sha1, Md5, Status, Data),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%%% Internal methods

store(Id, Sha1, Md5, Status, Data) ->
    Rec = #idx{id = Id, sha1 = to_db(Sha1), md5 = to_db(Md5),
               status = Status, data = Data},
    ok = mnesia:dirty_write(Rec).

%% Avoid uncontrolled expansion of secondary indexes
to_db(undefined) ->
    {A, B, _} = os:timestamp(),
    {A, B};
to_db(Sum) ->
    binary_to_integer(Sum, 16).


check(Sha1, Md5) ->
    check1(get_rec(Sha1, #idx.sha1), get_rec(Md5, #idx.md5)).

get_rec(undefined, _Pos) ->
    undefined;
get_rec(Sum, Pos) ->
    ISum = to_db(Sum),
    case mnesia:dirty_index_read(idx, ISum, Pos) of
        [#idx{sha1 = Sha1, md5 = Md5} = Rec] ->
            Rec#idx{sha1 = undummy(Sha1), md5 = undummy(Md5)};
        _ ->
            false
    end.

undummy({_,_}) -> undefined;
undummy(X) -> X.

check1(false, false) -> false;
check1(undefined, false) -> false;
check1(undefined, #idx{sha1 = undefined}) -> true;
check1(#idx{md5 = undefined}, undefined) -> true;
check1(Rec, Rec) -> true;
check1(_, _) -> false.


do_read(Sum) ->
    get_list(Sum, #idx.sha1) ++ get_list(Sum, #idx.md5).

from_db({_,_}) ->
    undefined;
from_db(Int) ->
    Bin = integer_to_binary(Int, 16),
    Fun = fun(X) when X >= $A, X =< $Z -> X + 32; (X) -> X end,
    << <<(Fun(X))>> || <<X>> <= Bin >>.

get_list(Sum, Pos) ->
    [format_rec(Rec) || Rec <- mnesia:dirty_index_read(idx, to_db(Sum), Pos)].

format_rec(#idx{sha1 = Sha1, md5 = Md5} = OldRec) ->
    Rec =  OldRec#idx{sha1 = from_db(Sha1), md5 = from_db(Md5)},
    yolf:to_map(record_info(fields, idx), Rec).

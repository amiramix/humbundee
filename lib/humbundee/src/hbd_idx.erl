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
         add/6,
         exists/3,
         read/1,
         delete/1
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

add(Id, Sha1, Md5, Size, Status, Data) ->
    gen_server:cast(?SERVER, {add, Id, Sha1, Md5, Size, Status, Data}).

exists(Sha1, Md5, Size) ->
    gen_server:call(?SERVER, {exists, Sha1, Md5, Size}).

read(Sum) ->
    gen_server:call(?SERVER, {read, Sum}).

delete(Sum) ->
    gen_server:call(?SERVER, {delete, Sum}).

%%------------------------------------------------------------------------------
%%% gen_server callbacks

init([]) ->
    ?LOG_WORKER_INIT(?SERVER),
    {ok, []}.

handle_call({exists, Sha1, Md5, Size}, _From, State) ->
    {reply, check(Sha1, Md5, Size), State};
handle_call({read, Sum}, _From, State) ->
    {reply, do_read(Sum), State};
handle_call({delete, Sum}, _From, State) ->
    {reply, do_delete(Sum), State};
handle_call(_, {Pid, _Tag}, State) ->
    exit(Pid, badarg),
    {noreply, State}.

handle_cast({add, Id, Sha1, Md5, Size, Status, Data}, State) ->
    store(Id, Sha1, Md5, Size, Status, Data),
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

store(_, undefined, undefined, _, _, _) ->
    ok;
store(Id, Sha1, Md5, Size, Status, Data) ->
    {ISha1, IMd5, Recs} = read_recs(Sha1, Md5),
    [mnesia:dirty_delete_object(R) ||
        R <- duplicates(ISha1, IMd5, Size, Recs, [])],

    Rec = #idx{id = Id, sha1 = to_store(ISha1), md5 = to_store(IMd5),
               size = Size, status = Status, data = Data},
    ok = mnesia:dirty_write(Rec).

read_recs(Sha1, Md5) ->
    ISha1 = to_int(Sha1),
    IMd5  = to_int(Md5),
    {ISha1, IMd5, get_recs(ISha1, #idx.sha1) ++ get_recs(IMd5, #idx.md5)}.

to_int(undefined) -> undefined;
to_int(Sum) -> binary_to_integer(Sum, 16).

get_recs(undefined, _Pos) -> [];
get_recs(ISum, Pos) -> mnesia:dirty_index_read(idx, ISum, Pos).

duplicates(Sha1, Md5, S,
           [#idx{sha1 = Sha1, md5 = Md5, size = S} = R|T], Acc) ->
    duplicates(Sha1, Md5, S, T, [R|Acc]);
duplicates(undefined = Sha1, Md5, S,
           [#idx{sha1 = {_,_}, md5 = Md5, size = S} = R|T], Acc) ->
    duplicates(Sha1, Md5, S, T, [R|Acc]);
duplicates(Sha1, undefined = Md5, S,
           [#idx{sha1 = Sha1, md5 = {_,_}, size = S} = R|T], Acc) ->
    duplicates(Sha1, Md5, S, T, [R|Acc]);
duplicates(Sha1, Md5, S, [_|T], Acc) ->
    duplicates(Sha1, Md5, S, T, Acc);
duplicates(_, _, _, [], Acc) ->
    Acc.

%% Avoid uncontrolled expansion of secondary indexes
to_store(undefined) ->
    {A, B, _} = os:timestamp(),
    {A, B};
to_store(Int) ->
    Int.


check(Sha1, Md5, Size) ->
    {ISha1, IMd5, Recs} = read_recs(Sha1, Md5),
    check(ISha1, IMd5, Size, Recs).

check( Sha1,  Md5,  S, [#idx{sha1 = Sha1,  md5 = Md5,   size = S}|_]) -> true;
check(_Sha1,  Md5,  S, [#idx{sha1 = {_,_}, md5 = Md5,   size = S}|_]) -> true;
check( Sha1, _Md5,  S, [#idx{sha1 = Sha1,  md5 = {_,_}, size = S}|_]) -> true;
check( Sha1,  Md5,  S, [_|T])  -> check(Sha1, Md5, S, T);
check(_Sha1, _Md5, _S, [])     -> false.

%%------------------------------------------------------------------------------

do_read(Sum) ->
    [format_rec(Rec) || Rec <- do_read1(is_hex(Sum), Sum)].

do_delete(Sum) ->
    [mnesia:dirty_delete_object(Rec) || Rec <- do_read1(is_hex(Sum), Sum)].


is_hex(<<X,T/binary>>)
  when X >= $0, X =< $9; X >= $a, X =< $f; X >= $A, X =< $F ->
    is_hex(T);
is_hex(<<_X,_/binary>>) ->
    false;
is_hex(<<>>) ->
    true.

format_rec(#idx{sha1 = Sha1, md5 = Md5} = OldRec) ->
    Rec =  OldRec#idx{sha1 = from_db(Sha1), md5 = from_db(Md5)},
    yolf:to_map(record_info(fields, idx), Rec).

from_db({_,_}) ->
    undefined;
from_db(Int) ->
    Bin = integer_to_binary(Int, 16),
    Fun = fun(X) when X >= $A, X =< $Z -> X + 32; (X) -> X end,
    << <<(Fun(X))>> || <<X>> <= Bin >>.

do_read1(false, Id) -> mnesia:dirty_read(idx, Id);
do_read1(true, Sum) -> read_sum(Sum, #idx.sha1) ++ read_sum(Sum, #idx.md5).

read_sum(Sum, Pos) -> mnesia:dirty_index_read(idx, to_int(Sum), Pos).

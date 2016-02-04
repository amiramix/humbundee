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

-module(humbundee).

-export([download/1, status/0, status/1, index/1, read/1, delete/1]).

download(Id) -> check(fun(X) -> hbd_api:download(X) end, Id).

status() -> hbd_api:status().

status(Id) -> check(fun(X) -> hbd_api:status(X) end, Id).

index(Id) -> check(fun(X) -> hbd_api:index(X) end, Id).

read(Sum) -> check(fun(X) -> hbd_idx:read(X) end, Sum).

delete(Sum) -> check(fun(X) -> hbd_idx:delete(X) end, Sum).

check(Fun, Val) ->
    Bin = yolf:to_binary(Val),
    case is_valid(Bin) of
        true -> Fun(Bin);
        {false, X} -> {error, {invalid_char, X}}
    end.

is_valid(<<X,T/binary>>)
  when X >= $0, X =< $9; X >= $a, X =< $z; X >= $A, X =< $Z ->
    is_valid(T);
is_valid(<<X,_/binary>>) ->
    {false, X};
is_valid(<<>>) ->
    true.

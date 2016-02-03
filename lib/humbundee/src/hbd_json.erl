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

-module(hbd_json).

-export([process/4]).

process(Url, Id, Cookie, OutDir) ->
    Json = get_json(Url, Id, Cookie, OutDir),
    {ok, Terms} = yajler:decode(Json),
    store_terms(OutDir, Id, Terms, <<".etr">>),
    Clean = parse(Terms),
    store_terms(OutDir, Id, Clean, <<"_clean.etr">>),
    Clean.

get_json(Url, Id, Cookie, Dir) ->
    FileName = filename:join(Dir, <<Id/binary, <<".json">>/binary>>),
    Cmd = << <<"wget -q --load-cookies ">>/binary, Cookie/binary,
             <<" -O ">>/binary, FileName/binary, <<" ">>/binary,
             Url/binary, Id/binary >>,
    yolog:tin(Cmd),
    case yexec:sh_cmd(Cmd) of
        {0, _} ->
            yolog:tin(<<"OK">>),
            ok;
        Err ->
            yolog:tin(<<"Error: ">>, Err),
            exit(no_json)
    end,
    {ok, Binary} = file:read_file(FileName),
    Binary.

store_terms(Dir, Id, Terms, Suffix) ->
    FileName = filename:join(Dir, <<Id/binary, Suffix/binary>>),
    ok = file:write_file(FileName,
                         io_lib:format("%% -*- erlang -*-~n~p.~n", [Terms])).

%%------------------------------------------------------------------------------

parse(Json) ->
    Main = proplists:get_value(<<"subproducts">>, Json),
    [cleanup(X) || X <- Main, is_downloads(X) =:= true].

is_downloads(List) ->
    case proplists:get_value(<<"downloads">>, List) of
        [] -> false;
        L when is_list(L) -> true
    end.

cleanup(X) ->
    Folder = clean_payee(proplists:get_value(<<"payee">>, X)),
    Title = proplists:get_value(<<"human_name">>, X),
    Downloads = clean_downloads(proplists:get_value(<<"downloads">>, X), []),
    #{folder => Folder, title => Title, downloads => Downloads}.

clean_payee(X) ->
    proplists:get_value(<<"human_name">>, X).

clean_downloads([H|T], Acc) -> clean_downloads(T, [clean_download(H)|Acc]);
clean_downloads([], Acc) -> Acc.

clean_download(X) ->
    MachineName = proplists:get_value(<<"machine_name">>, X),
    Platform = proplists:get_value(<<"platform">>, X),
    Structs = clean_structs(proplists:get_value(<<"download_struct">>, X), []),
    #{machname => MachineName, platform => Platform, structs => Structs}.

clean_structs([H|T], Acc) -> clean_structs(T, [clean_struct(H)|Acc]);
clean_structs([], Acc) -> Acc.

clean_struct(X) ->
    Name = proplists:get_value(<<"name">>, X),
    case proplists:get_value(<<"url">>, X) of
        Urls when is_list(Urls) -> clean_struct1(X, Name, Urls);
        undefined -> check_download(X, Name, #{url => undefined})
    end.

clean_struct1(X, Name, Urls) ->
    {Url, Torrent} = clean_url(Urls),
    Sha1 = proplists:get_value(<<"sha1">>, X),
    Md5 = proplists:get_value(<<"md5">>, X),
    Size = proplists:get_value(<<"file_size">>, X),
    #{sha1 => Sha1, md5 => Md5, size => Size,
      name => Name, url => Url, torrent => Torrent}.

clean_url(X) ->
    {proplists:get_value(<<"web">>, X),
     proplists:get_value(<<"bittorrent">>, X)}.

check_download(X, Name = <<"Stream">>, Struct) ->
    add_stream(get_stream_url(X), Struct#{name => Name});
check_download(_X, undefined, Struct) ->
    Struct.

add_stream(undefined, Struct) -> Struct;
add_stream(Url, Struct) -> Struct#{stream => Url}.

get_stream_url(X) ->
    case proplists:get_value(<<"hd_stream_url">>, X) of
        undefined -> proplists:get_value(<<"sd_stream_url">>, X);
        Url -> Url
    end.

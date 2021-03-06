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
    Urls = proplists:get_value(<<"url">>, X),
    clean_struct(X, init_struct(X, Name, Urls)).

init_struct(X, undefined, undefined) ->
    #{raw => X};
init_struct(X, Name, undefined) when is_binary(Name) ->
    #{raw => X, name => Name};
init_struct(X, Name, Urls) when is_binary(Name), is_list(Urls) ->
    {Url, Torrent} = clean_url(Urls),
    #{raw => X, url => Url, torrent => Torrent, name => Name}.

clean_url(X) ->
    {proplists:get_value(<<"web">>, X),
     proplists:get_value(<<"bittorrent">>, X)}.

clean_struct(X, #{url := _Url} = Struct) ->
    Sha1 = proplists:get_value(<<"sha1">>, X),
    Md5 = proplists:get_value(<<"md5">>, X),
    Size = proplists:get_value(<<"file_size">>, X),
    Struct#{sha1 => Sha1, md5 => Md5, size => Size};
clean_struct(X, Struct) ->
    do_struct(X, mapping(), Struct).

do_struct(X, [{Name, Prop, Key}|T], #{name := Name} = Struct) ->
    do_struct1(X, T, Struct, Prop, Key);
do_struct(X, [{undefined, Prop, Key}|T], Struct) ->
    do_struct1(X, T, Struct, Prop, Key);
do_struct(X, [_|T], Struct) ->
    do_struct(X, T, Struct);
do_struct(_X, [], Struct) ->
    Struct.

do_struct1(X, T, Struct, Prop, Key) ->
    case proplists:get_value(Prop, X) of
        undefined -> do_struct(X, T, Struct);
        Val when is_binary(Val) -> Struct#{Key => Val}
    end.

mapping() ->
    [{<<"Stream">>, <<"hd_stream_url">>, stream},
     {<<"Stream">>, <<"sd_stream_url">>, stream},
     {undefined,    <<"message">>,       message},
     {undefined,    <<"external_link">>, link}].

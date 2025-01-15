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

-module(hbd_cfg).

-export([setup/0]).

setup() ->
    check_deps(),
    #{regex := RegexL} = Cfg0 = add_user_config(read_config()),
    Cfg1 = Cfg0#{regex => process_regex(RegexL)},
    validate_cfg(Cfg1),
    Cfg1.

check_deps() ->
    IsWget  = is_cmd(<<"wget">>),
    All = [IsWget],
    Err = [X || {false, X} <- All],
    check_deps(Err).

is_cmd(App) ->
    yexec:is_cmd(App) orelse {false, App}.

check_deps([]) ->
    true;
check_deps(App) ->
    ylog:in(<<"Error: Humbundee requires that wget \n">>,
            <<"is installed in the system. The following applications ">>,
            <<"couldn't be found:">>),
    [ylog:i(<<X/binary, <<"\n">>/binary >>) || X <- App],
    throw(no_deps).

%%------------------------------------------------------------------------------

read_config() ->
    {ok, Url} = application:get_env(humbundee, order_url),
    {ok, Coo} = application:get_env(humbundee, cookie),
    {ok, Dir} = application:get_env(humbundee, download_dir),
    {ok, Tmp} = application:get_env(humbundee, temp_dir),
    {ok, Des} = application:get_env(humbundee, destination_dir),
    Wrk = application:get_env(humbundee, workers, 20),
    Cfg = #{url     => yolf:to_binary(Url),
            cookie  => yolf:to_binary(Coo),
            in      => yolf:to_binary(Dir),
            tmp     => yolf:to_binary(Tmp),
            dest    => yolf:to_binary(Des),
            regex   => application:get_env(humbundee, exclude_regex_list, []),
            workers => yolf:to_integer(Wrk)},
    ylog:in(<<"Read config:">>, endl, Cfg, endl),
    Cfg.

add_user_config(Cfg) ->
    Name = application:get_env(humbundee, user_config, <<".humbundee.conf">>),
    File = filename:join(get_home(), Name),
    case file:consult(File) of
        {ok, UserCfg} ->
            ylog:in(<<"Read user config:">>, endl, UserCfg, endl),
            merge_configs(Cfg, UserCfg);
        {error, _} ->
            ylog:in(<<"Can't read user config '">>, File, <<"'. Ignoring...">>),
            Cfg
    end.

get_home() ->
    case os:getenv("HOME") of
        false -> no_home();
        Home -> Home
    end.

no_home() ->
    ylog:in(<<"Error: Environment variable 'HOME' not set!">>),
    throw(no_home).

merge_configs(Cfg0, UserCfg) ->
    Cfg1 = add_cfg(url,     Cfg0, order_url,          UserCfg, to_binary),
    Cfg2 = add_cfg(cookie,  Cfg1, cookie,             UserCfg, to_binary),
    Cfg3 = add_cfg(in,      Cfg2, download_dir,       UserCfg, to_binary),
    Cfg4 = add_cfg(tmp,     Cfg3, temp_dir,           UserCfg, to_binary),
    Cfg5 = add_cfg(dest,    Cfg4, destination_dir,    UserCfg, to_binary),
    Cfg6 = add_cfg(regex,   Cfg5, exclude_regex_list, UserCfg, undefined),
    _Dum = add_cfg(workers, Cfg6, workers,            UserCfg, to_integer).

add_cfg(MapKey, Map, CfgKey, Cfg, ConvFun) ->
    case proplists:get_value(CfgKey, Cfg) of
        undefined -> Map;
        Value -> Map#{MapKey => convert(ConvFun, Value)}
    end.

convert(undefined, Value) -> Value;
convert(ConvFun, Value)   -> yolf:ConvFun(Value).

%%------------------------------------------------------------------------------

process_regex(RegexList) -> [compile_re(X) || X <- RegexList].

compile_re({name, Re})           -> compile_re1({name, Re, []});
compile_re({name, _, _} = R)     -> compile_re1(R);
compile_re({link, Re})           -> compile_re1({link, Re, []});
compile_re({link, _, _} = R)     -> compile_re1(R);
compile_re({platform, Re})       -> compile_re1({platform, Re, []});
compile_re({platform, _, _} = R) -> compile_re1(R);
compile_re({Re, Opts})           -> compile_re1({any, Re, Opts});
compile_re(Re)                   -> compile_re({Re, []}).

compile_re1({Atom, Re, Opts}) ->
    {ok, MP} = re:compile(Re, Opts),
    {Atom, MP, lists:filter(fun filter_opts/1, Opts)}.

filter_opts(anchored)                   -> true;
filter_opts(global)                     -> true;
filter_opts(notbol)                     -> true;
filter_opts(noteol)                     -> true;
filter_opts(report_errors)              -> true;
filter_opts(notempty)                   -> true;
filter_opts(notempty_atstart)           -> true;
filter_opts({offset, _})                -> true;
filter_opts({match_limit, _})           -> true;
filter_opts({match_limit_recursion, _}) -> true;
filter_opts({newline, _})               -> true;
filter_opts({capture, _})               -> true;
filter_opts({capture, _, _})            -> true;
filter_opts(_)                          -> false.

%%------------------------------------------------------------------------------

validate_cfg(#{cookie := Cookie, in := In, tmp := Tmp, dest := Dest}) ->

    case filelib:is_regular(Cookie) of
        true -> ok;
        false -> no_cookie(Cookie)
    end,
    ensure_dir(In, <<"Download">>, bad_download_dir),
    ensure_dir(Tmp, <<"Temp">>, bad_temp_dir),
    ensure_dir(Dest, <<"Destination">>, bad_destination_dir),
    check_empty(In, <<"Download">>, download_dir_not_empty),
    check_empty(Tmp, <<"Temp">>, temp_dir_not_empty).

no_cookie(Cookie) ->
    ylog:in(<<"Cookie file '">>, Cookie, <<"' doesn't exist, exiting.">>),
    throw(no_cookie).

ensure_dir(Dir, Name, Type) ->
    case ycmd:ensure_dir(Dir) of
        ok -> ok;
        {error, _} = Err -> no_dir(Name, Type, Dir, Err)
    end.

no_dir(Name, Type, Dir, Err) ->
    ylog:in(Name, <<" directory '">>, Dir, <<", doesn't exist and couldn't ">>,
            <<" be created, error: ">>, Err, <<". Exiting.">>),
    throw(Type).

check_empty(Dir, Name, Type) ->
    case ycmd:ls_dir(Dir) of
        [] -> ok;
        _ -> not_empty(Name, Type, Dir)
    end.

not_empty(Name, Type, Dir) ->
    ylog:in(Name, <<" directory '">>, Dir, <<", is not empty. Exiting.">>),
    throw(Type).

%%------------------------------------------------------------------------------


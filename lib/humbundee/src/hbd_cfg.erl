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

-module(hbd_cfg).

-export([setup/0]).

setup() ->
    check_deps(),
    Cfg0 = read_config(),
    Cfg1 = add_user_config(Cfg0),
    validate_cfg(Cfg1),
    start_qbt(),
    Cfg1.

check_deps() ->
    IsWget  = is_cmd(<<"wget">>),
    IsTrans = is_cmd(<<"transmission-show">>),
    IsBt    = is_cmd(<<"qbittorrent-nox">>),
    All = [IsWget, IsTrans, IsBt],
    Err = [X || {false, X} <- All],
    check_deps(Err).

is_cmd(App) ->
    yexec:is_cmd(App) orelse {false, App}.

check_deps([]) ->
    true;
check_deps(App) ->
    yio:errorn(<<"Error: Humbundee requires that wget, transmission-show ">>,
               <<"(part of transmission-cli) and qbittorrent-nox are\n">>,
               <<"installed in the system. The following applications ">>,
               <<"couldn't be found:">>),
    [yio:error(<<X/binary, <<"\n">>/binary >>) || X <- App],
    throw(no_deps).

%%------------------------------------------------------------------------------

read_config() ->
    {ok, Url} = application:get_env(humbundee, order_url),
    {ok, Coo} = application:get_env(humbundee, cookie),
    {ok, Dir} = application:get_env(humbundee, download_location),
    Exc = application:get_env(humbundee, exclude_regex_list, []),
    Wrk = application:get_env(humbundee, workers, 20),
    Cfg = #{url     => Url,
            cookie  => Coo,
            dir     => Dir,
            regex   => Exc,
            workers => Wrk},
    yio:in(<<"Read config:">>, endl, Cfg, endl),
    Cfg.


add_user_config(Cfg) ->
    Name = application:get_env(humbundee, user_config, <<".humbundee.conf">>),
    File = filename:join(get_home(), Name),
    case file:consult(File) of
        {ok, UserCfg} ->
            yio:in(<<"Read user config:">>, endl, UserCfg, endl),
            merge_configs(Cfg, UserCfg);
        {error, _} ->
            yio:en(<<"Can't read user config '">>, File, <<"'. Ignoring...">>),
            Cfg
    end.

get_home() ->
    case os:getenv(<<"HOME">>) of
        false -> no_home();
        Home -> Home
    end.

no_home() ->
    yio:en(<<"Error: Environment variable 'HOME' not set!">>),
    throw(no_home).

merge_configs(Cfg0, UserCfg) ->
    Cfg1 = add_cfg(url,    Cfg0, order_url,          UserCfg),
    Cfg2 = add_cfg(cookie, Cfg1, cookie,             UserCfg),
    Cfg3 = add_cfg(dir,    Cfg2, download_location,  UserCfg),
    Cfg4 = add_cfg(regex,  Cfg3, exclude_regex_list, UserCfg),
    add_cfg(workers, Cfg4, workers, UserCfg).

add_cfg(MapKey, Map, CfgKey, Cfg) ->
    case proplists:get_value(CfgKey, Cfg) of
        undefined -> Map;
        Value -> Map#{MapKey => Value}
    end.

%%------------------------------------------------------------------------------

validate_cfg(#{cookie := Cookie, dir := Dir}) ->
    case filelib:is_regular(Cookie) of
        true -> ok;
        false -> no_cookie(Cookie)
    end,
    case filelib:ensure_dir(Dir) of
        ok -> ok;
        {error, _} = Err -> no_download_dir(Dir, Err)
    end.

no_cookie(Cookie) ->
    yio:en(<<"Cookie file '">>, Cookie, <<"' doesn't exist, exiting.">>),
    throw(no_cookie).

no_download_dir(Dir, Err) ->
    yio:en(<<"Download directory '">>, Dir, <<", doesn't exist and couldn't ">>,
           <<" be created, error: ">>, Err, <<". Exiting.">>),
    throw(bad_dir).

%%------------------------------------------------------------------------------

start_qbt() ->
    yio:i(<<"Starting qbittorrent daemon... ">>),
    case yexec:sh_cmd(<<"qbittorrent-nox -d">>) of
        {0, _} ->
            yio:in(<<"OK">>),
            ok;
        {1, _} ->
            yio:in(<<"Already started.">>),
            ok;
        Err ->
            yio:en(<<"Error: ">>, Err),
            throw(no_qbt)
    end.

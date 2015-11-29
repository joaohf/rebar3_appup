%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar3_appup_clean).

-behaviour(provider).

-export([init/1,
	 do/1,
	 format_error/1]).

-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, clean},
                                 {module, ?MODULE},
                                 {namespace, appup},
                                 {deps, [{default, app_discovery}]},
                                 {bare, true},
                                 {example, "rebar3 appup clean"},
                                 {short_desc, "Cleanup appup .appup files."},
                                 {desc, "Cleanup appup .appup files."}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [do_clean(appup_file(AppInfo)) || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format(Reason).

do_clean(TargetFile) ->
    ec_file:remove(TargetFile, []).

appup_file(AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(OutDir, ec_cnv:to_list(Name) ++ ".appup").

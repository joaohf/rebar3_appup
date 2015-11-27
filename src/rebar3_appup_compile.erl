%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar3_appup_compile).

-behaviour(provider).

-export([init/1,
	 do/1,
	 format_error/1]).

-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, compile},
                                 {module, ?MODULE},
                                 {namespace, appup},
                                 {deps, [{default, compile}]},
                                 {bare, true},
                                 {example, "rebar3 appup compile"},
                                 {short_desc, "Compile appup .appup.src files."},
                                 {desc, "Compile appup .appup.src files"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [do_appup(appup_file_src(AppInfo), appup_file(AppInfo)) || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({failed_writing_target, [TargetFile, Reason]}) ->
    io_lib:format("Failed writing to target file ~s due to ~s", [TargetFile, Reason]);
format_error({failed_to_compile, [SourceFile, Reason]}) ->
    io_lib:format("Failed to compile ~s: ~p~n", [SourceFile, Reason]);
format_error({failed_to_compile_not_appup, [SourceFile]}) ->
    io_lib:format("Failed to compile ~s, not an appup~n", [SourceFile]).

do_appup(SourceFile, TargetFile) ->
    %% Perform basic validation on the appup file
    %% i.e. if a consult succeeds and basic appup
    %% structure exists.
    case file:consult(SourceFile) of
        %% The .appup syntax is described in
        %% http://erlang.org/doc/man/appup.html.
        {ok, [{_Vsn, [_UpFromVsn], [_DownToVsn]} = AppUp]} ->
            case file:write_file(TargetFile,
                                 lists:flatten(io_lib:format("~p.", [AppUp]))) of
                {error, Reason} ->
                    throw(?PRV_ERROR({failed_writing_target, [TargetFile, Reason]}));
                ok -> ok
            end;
        {error, Reason} ->
            throw(?PRV_ERROR({failed_to_compile, [SourceFile, Reason]}));
        _ ->
            throw(?PRV_ERROR({failed_to_compile_not_appup, [SourceFile]}))
    end.

appup_file_src(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join([Dir, "src", ec_cnv:to_list(Name) ++".appup.src"]).

appup_file(AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(OutDir, ec_cnv:to_list(Name) ++ ".appup").

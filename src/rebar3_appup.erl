%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar3_appup).

-behaviour(provider).

-export([init/1,
	 do/1,
	 format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, "appup"},
                                 {module, ?MODULE},
                                 {deps, [compile]},
                                 {bare, true},
                                 {example, undefined},
                                 {desc, "appup files"}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [process(appup_file_src(AppInfo), appup_file(AppInfo)) || AppInfo <- Apps],
     
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({writing_target_failed, [TargetFile, Reason]}) ->
    io_lib:format("Failed writing to target file ~s due to ~s", [TargetFile, Reason]);
format_error({failed_to_compile, [SourceFile, Reason]}) ->
    io_lib:format("Failed to compile ~s: ~p~n", [SourceFile, Reason]);
format_error({failed_to_compile_not_appup, [SourceFile]}) ->
    io_lib:format("Failed to compile ~s, not an appup~n", [SourceFile]).

process(SourceFile, TargetFile) ->
    %% Perform basic validation on the appup file
    %% i.e. if a consult succeeds and basic appup
    %% structure exists.
    case rebar_config:consult_file(SourceFile) of
        %% The .appup syntax is described in
        %% http://erlang.org/doc/man/appup.html.
        {ok, [{_Vsn, [_UpFromVsn], [_DownToVsn]} = AppUp]} ->
            case file:write_file(TargetFile,
                                 lists:flatten(io_lib:format("~p.", [AppUp]))) of
                {error, Reason} ->
                    {error, {writing_target_failed, [TargetFile, Reason]}};
                ok -> ok
            end;
        {error, Reason} ->
            {error, {failed_to_compile, [SourceFile, Reason]}};
        _ ->
            {error, {failed_to_compile_not_appup, [SourceFile]}}
    end.


appup_file_src(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(Dir, Name++ ".appup.src").

appup_file(AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),
    Name = rebar_app_info:name(AppInfo),
    filename:join(OutDir, Name ++ ".appup").

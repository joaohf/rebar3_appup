%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar3_appup).

-export([init/1]).

-define(PRV_ERROR(Reason), {error, {?MODULE, Reason}}).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_appup_compile:init(State),
    {ok, State2} = rebar3_appup_clean:init(State1),
    {ok, State2}.

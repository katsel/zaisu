-module(zaisu_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPS, [crypto, cowlib, ranch, cowboy, zaisu]).


% Test descriptions

start_stop_test_() ->
    {
        "The server can be started and stopped and is alive in between",
        {
            setup,
            fun start/0, fun stop/1,
            fun(_) ->
                is_app_alive(zaisu_sup)
            end
        }
    }.


% Setup functions

start() ->
    start_applications(?APPS).

stop(_) ->
    ok = stop_applications(?APPS).


% Actual tests

hello_test() ->
    ok.

is_app_alive(App) ->
    Pid = whereis(App),
    ?_assert(erlang:is_process_alive(Pid)).


% Helper functions

start_applications(Apps) ->
    start_applications(Apps, []).

start_applications([], Acc) ->
    lists:reverse(Acc);
start_applications([App|Apps], Acc) ->
    case application:start(App) of
    {error, {already_started, _}} ->
        start_applications(Apps, Acc);
    {error, {not_started, Dep}} ->
        start_applications([Dep, App | Apps], Acc);
    {error, {not_running, Dep}} ->
        start_applications([Dep, App | Apps], Acc);
    ok ->
        start_applications(Apps, [App|Acc])
    end.

stop_applications(Apps) ->
    [application:stop(App) || App <- lists:reverse(Apps)],
    ok.

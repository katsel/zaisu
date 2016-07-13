-module(zaisu_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPS, [crypto, cowlib, ranch, cowboy, zaisu]).


%%% Test descriptions

start_stop_test_() ->
    { "The server can be started and stopped and is alive in between",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            app_is_alive(zaisu_sup)
        end
        }
    }.

index_test_() ->
    { "The index page looks as expected",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            index_exists()
        end
        }
    }.

db_can_be_created_test_() ->
    { "A database can be created",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            db_can_be_created()
        end
        }
    }.

no_duplicate_db_test_() ->
    { "The same database cannot be created twice",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            create_conflicting_dbs()
        end
        }
    }.


%%% Setup functions

start() ->
    start_applications(?APPS).

stop(_) ->
    ok = stop_applications(?APPS).


%%% Actual tests

hello_test() ->
    ok.

app_is_alive(App) ->
    Pid = whereis(App),
    ?_assert(erlang:is_process_alive(Pid)).

index_exists() ->
    {Status, _, Body} = do_get("/"),
    [?_assertEqual(200, Status),
     ?_assertEqual(<<"{\"zaisu\":\"Welcome\"}\n">>, Body)].

db_can_be_created() ->
    TestDbPath = "/testdb",
    {PutStatus, _, PutBody} = do_put(TestDbPath),
    {GetStatus, _, GetBody} = do_get(TestDbPath),
    {AllDbStatus, _, AllDbBody} = do_get("/_all_dbs"),
    [[?_assertEqual(201, PutStatus),
      ?_assertEqual(<<"\{\"ok\":true\}\n">>, PutBody)],
     [?_assertEqual(200, GetStatus),
      ?_assertEqual(<<"\{\"db_name\":\"testdb\"\}\n">>, GetBody)],
     [?_assertEqual(200, AllDbStatus),
      ?_assertEqual(<<"[\"testdb\"]\n">>, AllDbBody)]].

create_conflicting_dbs() ->
    {Status1, _, _} = do_put("/testdb"),
    {Status2, _, _} = do_put("/testdb"),
    [?_assertEqual(201, Status1),
     ?_assertEqual(409, Status2)].


%%% Helper functions

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

gun_open() ->
    {ok, ConnPid} = gun:open("localhost", 8080, #{retry => 0}),
    ConnPid.

%% generic function to handle the response of a HTTP request
handle_gun_response(ConnPid, Ref) ->
    case gun:await(ConnPid, Ref) of
        {response, nofin, Status, RespHeaders} ->
            {ok, Body} = gun:await_body(ConnPid, Ref),
            {Status, RespHeaders, Body};
        {response, fin, Status, RespHeaders} ->
            {Status, RespHeaders, <<>>}
    end.

%% send out a GET request and fetch its response
do_get(Path) ->
    ConnPid = gun_open(),
    Ref = gun:get(ConnPid, Path),
    handle_gun_response(ConnPid, Ref).

%% send out a PUT request and fetch its response
do_put(Path) ->
    ConnPid = gun_open(),
    Ref = gun:put(ConnPid, Path, []),
    handle_gun_response(ConnPid, Ref).

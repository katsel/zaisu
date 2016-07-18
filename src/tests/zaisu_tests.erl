-module(zaisu_tests).
-include_lib("eunit/include/eunit.hrl").

-define(APPS,
    [crypto, cowlib, ranch, cowboy, zaisu, asn1, public_key, ssl, gun]).


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

db_can_be_deleted_test_() ->
    [{ "Deleting a nonexistent database results in an error",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            delete_nonexistent_db()
        end
        }
     },
     { "A database can be deleted",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            create_delete_db()
        end
        }
    }].

illegal_dbname_test_() ->
    { "Cannot PUT/GET/DELETE a database with an illegal name",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            check_illegal_dbname()
        end
        }
    }.


%%% Setup functions

start() ->
    start_applications(?APPS).

stop(_) ->
    ok = stop_applications(?APPS).


%%% Actual tests

app_is_alive(App) ->
    Pid = whereis(App),
    ?_assert(erlang:is_process_alive(Pid)).

index_exists() ->
    {Status, _, Body} = do_get("/"),
    [?_assertEqual(200, Status),
     ?_assert(index_has_welcome(Body))].

db_can_be_created() ->
    TestDbPath = "/testdb",
    {PutStatus, _, PutBody} = do_put(TestDbPath),
    {HeadStatus, _, HeadBody} = do_head(TestDbPath),
    {GetStatus, _, GetBody} = do_get(TestDbPath),
    {AllDbStatus, _, AllDbBody} = do_get("/_all_dbs"),
    {_, _, _} = do_delete(TestDbPath),
    [[?_assertEqual(201, PutStatus),
      ?_assertEqual(<<"\{\"ok\":true\}\n">>, PutBody)],
     [?_assertEqual(200, HeadStatus),
      ?_assertEqual(<<>>, HeadBody)],
     [?_assertEqual(200, GetStatus),
      ?_assert(dbpage_is_valid(GetBody, <<"testdb">>))],
     [?_assertEqual(200, AllDbStatus),
      ?_assert(alldbs_has_database(AllDbBody, <<"testdb">>))]].

create_conflicting_dbs() ->
    {Status1, _, _} = do_put("/testdb"),
    {Status2, _, Body2} = do_put("/testdb"),
    {_, _, _} = do_delete("/testdb"),
    ErrorMsg = <<"\{\"error\":\"file_exists\",\"reason\":\"The database ",
        "could not be created, the file already exists.\"\}\n">>,
    [?_assertEqual(201, Status1),
     ?_assertEqual(412, Status2),
     ?_assertEqual(ErrorMsg, Body2)].

delete_nonexistent_db() ->
    {Status, _, _} = do_delete("/abcd"),
    [?_assertEqual(404, Status)].

create_delete_db() ->
    {_, _, _} = do_put("/abcd"),
    {Status, _, Body} = do_delete("/abcd"),
    {_, _, AllDbBody} = do_get("/_all_dbs"),
    [?_assertEqual(200, Status),
     ?_assertEqual(<<"\{\"ok\":true\}\n">>, Body),
     ?_assert(alldbs_is_empty(AllDbBody))].

check_illegal_dbname() ->
    {PutStatus, _, PutBody} = do_put("/1abc"),
    {HeadStatus, _, HeadBody} = do_head("/1abc"),
    {GetStatus, _, GetBody} = do_get("/1abc"),
    {DeleteStatus, _, DeleteBody} = do_delete("/1abc"),
    ErrorMsg = <<"\{\"error\":\"illegal_database_name\",\"reason\":",
        "\"Name: '1abc'. Only lowercase characters (a-z), digits (0-9), and ",
        "any of the characters _, $, (, ), +, -, and / are allowed. Must ",
        "begin with a letter.\"\}\n">>,
    [[?_assertEqual(400, PutStatus),
      ?_assertEqual(ErrorMsg, PutBody)],
     [?_assertEqual(400, HeadStatus),
      ?_assertEqual(<<>>, HeadBody)],
     [?_assertEqual(400, GetStatus),
      ?_assertEqual(ErrorMsg, GetBody)],
     [?_assertEqual(400, DeleteStatus),
      ?_assertEqual(ErrorMsg, DeleteBody)]].


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
    Host = os:getenv("HOST", "localhost"),
    {Port, _} = string:to_integer(os:getenv("PORT", "8080")),
    {ok, ConnPid} = gun:open(Host, Port, #{retry => 0}),
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

%% send out a HEAD request and fetch its response
do_head(Path) ->
    ConnPid = gun_open(),
    Ref = gun:head(ConnPid, Path),
    handle_gun_response(ConnPid, Ref).

%% send out a PUT request and fetch its response
do_put(Path) ->
    ConnPid = gun_open(),
    Ref = gun:put(ConnPid, Path, []),
    handle_gun_response(ConnPid, Ref).

%% send out a DELETE request and fetch its response
do_delete(Path) ->
    ConnPid = gun_open(),
    Ref = gun:delete(ConnPid, Path),
    handle_gun_response(ConnPid, Ref).

%% check if the index page has a welcome message
index_has_welcome(IndexBody) ->
    Index = element(1, jiffy:decode(IndexBody)),
    (lists:keyfind(<<"couchdb">>, 1, Index) == {<<"couchdb">>, <<"Welcome">>})
        or
        (lists:keyfind(<<"zaisu">>, 1, Index) == {<<"zaisu">>, <<"Welcome">>}).

%% check if DbName is in the ResponseBody of _all_dbs
alldbs_has_database(ResponseBody, DbName) when DbName == [] ->
    case ResponseBody of
        <<"[\"_replicator\",\"_users\"]\n">> -> true;  % CouchDB
        <<"[]\n">> -> true;                            % Zaisu
        _ -> false
    end;
alldbs_has_database(ResponseBody, DbName) ->
    case re:run(ResponseBody, DbName, [{capture, none}, dollar_endonly]) of
        match -> true;
        nomatch -> false
    end.

alldbs_is_empty(ResponseBody) ->
    alldbs_has_database(ResponseBody, []).

%% check if the /dbname page is valid
dbpage_is_valid(ResponseBody, DbName) ->
    DbPage = element(1, jiffy:decode(ResponseBody)),
    DbNameTuple = lists:keyfind(<<"db_name">>, 1, DbPage),
    DbNameTuple == {<<"db_name">>, DbName}.

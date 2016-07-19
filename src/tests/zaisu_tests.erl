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

nonexistent_db_test_() ->
    { "Cannot GET a database that does not exist",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            get_nonexistent_db()
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

doc_can_be_created_test_() ->
    [{ "A document cannot be created without creating a database first",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            doc_without_db()
        end
        }
     },
     { "A document can be created inside a database",
        {
        setup,
        fun start/0, fun stop/1,
        fun(_) ->
            doc_can_be_created()
        end
        }
    }].


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
     ?_assert(responsebody_has_value(Body, <<"Welcome">>))].

get_nonexistent_db() ->
    {Status, _, Body} = do_get("/nodbhere"),
    [?_assertEqual(404, Status),
     ?_assertEqual(
        <<"{\"error\":\"not_found\",\"reason\":\"no_db_file\"}\n">>, Body)].

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
      ?_assertEqual(
        responsebody_get_value(GetBody, <<"db_name">>), <<"testdb">>)
     ],
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
    {Status, _, Body} = do_delete("/abcd"),
    ErrorMsg = <<"{\"error\":\"not_found\",\"reason\":\"missing\"}\n">>,
    [?_assertEqual(404, Status),
     ?_assertEqual(ErrorMsg, Body)].

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

doc_without_db() ->
    {Status, _, Body} = do_post("/testdb", "{\"Company\":\"Example, Inc.\"}"),
    [?_assertEqual(404, Status),
     ?_assertEqual(
        <<"{\"error\":\"not_found\",\"reason\":\"no_db_file\"}\n">>, Body)].

doc_can_be_created() ->
    {_, _, _} = do_put("/testdb"),
    {Status, _, Body} = do_post("/testdb", "{\"Company\":\"Example, Inc.\"}"),
    {_, _, _} = do_delete("/testdb"),
    JBody = jiffy:decode(Body, [return_maps]),
    [?_assertEqual(201, Status),
     ?_assertEqual(true, responsebody_get_value(JBody, <<"ok">>)),
     ?_assert(responsebody_has_key(JBody, <<"id">>)),
     ?_assertEqual(
        responsebody_get_value(JBody, <<"rev">>),
        <<"1-3f23f12349a5e83d318d48bef42f6034">>
     )].


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

%% send a POST request with a JSON body and fetch its response
do_post(Path, Body) ->
    ConnPid = gun_open(),
    Ref = gun:post(ConnPid, Path, [
        {<<"content-type">>, "application/json"}
    ], Body),
    handle_gun_response(ConnPid, Ref).

%% check if DbName is in the ResponseBody of /_all_dbs
alldbs_has_database(ResponseBody, DbName) ->
    JRespBody = jiffy:decode(ResponseBody),
    lists:member(DbName, JRespBody).

%% check if /_all_dbs is empty (except for system databases)
alldbs_is_empty(ResponseBody) ->
    JRespBody = jiffy:decode(ResponseBody),
    case JRespBody of
        [] -> true;  % zaisu
        [<<"_replicator">>, <<"_users">>] -> true;  % CouchDB
        _ -> false
    end.

%% check if the response body contains a certain value (regardless of key)
responsebody_has_value(ResponseBody, Value) ->
    Index = element(1, jiffy:decode(ResponseBody)),
    case lists:keyfind(Value, 2, Index) of
        {_, Value} -> true;
        _ -> false
    end.

%% return value for key from the response body
responsebody_get_value(ResponseBody, Key) when is_binary(ResponseBody) ->
    JRespBody = jiffy:decode(ResponseBody, [return_maps]),
    responsebody_get_value(JRespBody, Key);
responsebody_get_value(JRespBody, Key) ->
    maps:get(Key, JRespBody).

%% check if the response body contains a certain key
responsebody_has_key(ResponseBody, Key) when is_binary(ResponseBody) ->
    JRespBody = jiffy:decode(ResponseBody, [return_maps]),
    responsebody_has_key(JRespBody, Key);
responsebody_has_key(JRespBody, Key) ->
    case maps:find(Key, JRespBody) of
        error -> false;
        {ok, _} -> true
    end.

%%% @doc Database handler.
-module(db_handler).

%%% Standard callbacks
-export([init/2]).
-export([allow_missing_post/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_completed/2]).
-export([delete_resource/2]).
-export([is_conflict/2]).
-export([resource_exists/2]).

%%% Custom callbacks
-export([dbinfo_to_json/2]).
-export([create_db/2]).
-export([create_document/2]).

%% valid database name
-define(DBNAME_REGEX,
    "^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*"  % stock CouchDB regex
).


init(Req, DbList) ->
    {cowboy_rest, Req, DbList}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"PUT">>, <<"DELETE">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, create_document},
        {'*', create_db}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, dbinfo_to_json},       % JSON content as plaintext
        {<<"application/json">>, dbinfo_to_json}  % JSON content as JSON
    ], Req, State}.

delete_completed(Req, DbList) ->
    Response = jiffy:encode({[{ok, true}]}),
    Req2 = cowboy_req:reply(200, #{}, <<Response/binary, "\n">>, Req), % 200 OK
    {true, Req2, DbList}.

delete_resource(Req, DbList) ->
    DbName = cowboy_req:binding(db_name, Req),
    {delete_db(DbName, DbList), Req, DbList}.

is_conflict(Req, DbList) ->
    DbName = cowboy_req:binding(db_name, Req),
    case db_exists(DbName, DbList) of
        true ->
            Response = jiffy:encode({[
                {error, file_exists},
                {reason, <<"The database could not be created, the file ",
                    "already exists.">>}
                ]}),
            Req2 = cowboy_req:reply(412, #{}, <<Response/binary, "\n">>, Req),
            {true, Req2, DbList};
        false ->
            {false, Req, DbList}
    end.

resource_exists(Req, DbList) ->
    DbName = cowboy_req:binding(db_name, Req),
    case validate_dbname(DbName) of
        ok ->
            DbExists = db_exists(DbName, DbList),
            handle_resource_request(DbExists, Req, DbList);
        _ ->
            Req2 = cowboy_req:reply(400, #{},  % 400 Bad Request
                illegal_dbname_warning(DbName), Req),
            {false, Req2, DbList}
    end.


dbinfo_to_json(Req, DbList) ->
    DbName = cowboy_req:binding(db_name, Req),
    Response = jiffy:encode({[{db_name, DbName}]}),
    {<<Response/binary, "\n">>, Req, DbList}.

create_db(Req, DbList) ->
    DbName = cowboy_req:binding(db_name, Req),
    add_db(DbName, DbList),
    Response = jiffy:encode({[{ok, true}]}),
    Req2 = cowboy_req:reply(201, #{},  % 201 Created
        <<Response/binary, "\n">>, Req),
    {true, Req2, DbName}.

create_document(Req, DbList) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Method = cowboy_req:method(Req2),
    HasBody = cowboy_req:has_body(Req2),
    Encoding = cowboy_req:parse_header(<<"transfer-encoding">>, Req2),
    Length = cowboy_req:parse_header(<<"content-length">>, Req2),
    DbName = cowboy_req:binding(db_name, Req2),
    AllHeaders = cowboy_req:headers(Req2),
    Response = jiffy:encode({[
        {db_name, DbName},
        {method, Method},
        {has_body, HasBody},
        {encoding, Encoding},
        {body_length, Length}
        ]}),
    io:format(Body),                      %% debug
    io:format(Response),                  %% debug
    io:format(jiffy:encode(AllHeaders)),  %% debug
    Req3  = cowboy_req:set_resp_body(<<Response/binary, "\n">>, Req2),
    {true, Req3, DbList}.


% Private

add_db(DbName, DbList) ->
    ets:insert_new(DbList, {DbName}).

db_exists(DbName, DbList) ->
    ets:member(DbList, DbName).

delete_db(DbName, DbList) ->
    ets:delete(DbList, DbName).

handle_resource_request(true, Req, DbList) ->
    {true, Req, DbList};
handle_resource_request(false, Req, DbList) ->
    Reason = case cowboy_req:method(Req) of
        <<"DELETE">> -> missing;  % needed for consistency with CouchDB
        _ -> no_db_file
    end,
    ResponseBody = jiffy:encode({[
        {error, not_found},
        {reason, Reason}
    ]}),
    Req2 = cowboy_req:set_resp_body(<<ResponseBody/binary, "\n">>, Req),
    {false, Req2, DbList}.

illegal_dbname_warning(DbName) ->
    Response = jiffy:encode({[
        {error, illegal_database_name},
        {reason, <<"Name: '", DbName/binary, "'. ",
            "Only lowercase characters (a-z), digits (0-9), and any of the ",
            "characters _, $, (, ), +, -, and / are allowed. ",
            "Must begin with a letter.">>}
    ]}),
    <<Response/binary, "\n">>.

validate_dbname(DbName) when is_binary(DbName) ->
    case re:run(DbName, ?DBNAME_REGEX, [{capture,none}, dollar_endonly]) of
        match ->
            ok;
        nomatch ->
            {error, {illegal_database_name, DbName}}
    end.

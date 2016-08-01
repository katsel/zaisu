%%% @doc Handler for /_all_dbs end point.
-module(all_dbs_handler).

%%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%%% Custom callbacks
-export([all_dbs_to_json/2]).


init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Response = jiffy:encode({[
        {error, method_not_allowed},
        {reason, <<"Only GET,HEAD allowed">>}
    ]}),
    Req2 = cowboy_req:set_resp_body(<<Response/binary, "\n">>, Req),
    {[<<"GET">>, <<"HEAD">>], Req2, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, all_dbs_to_json},
        {<<"application/json">>, all_dbs_to_json}
    ], Req, State}.


all_dbs_to_json(Req, DbList) ->
    % database names are stored as the keys of the 'DbList' ets
    AllDbsAsList = keys(DbList),
    Response = jiffy:encode(AllDbsAsList),
    {<<Response/binary, "\n">>, Req, DbList}.


%% retrieve all keys of an ets object
keys(TableName) ->
    FirstKey = ets:first(TableName),
    Keys = keys(TableName, FirstKey, [FirstKey]),
    lists:reverse(Keys).

keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
    Acc;
keys(TableName, CurrentKey, Acc) ->
    NextKey = ets:next(TableName, CurrentKey),
    keys(TableName, NextKey, [NextKey|Acc]).

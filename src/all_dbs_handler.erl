%% @doc _all_dbs handler.
-module(all_dbs_handler).

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Custom callbacks
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
    AllDbsAsList = lists:map(
        % ets stores dbnames as bitstrings inside tuples. de-tuplify
        fun(X) -> {Y} = X, Y end,
        lists:usort(ets:tab2list(DbList))),
    Response = jiffy:encode(AllDbsAsList),
    {<<Response/binary, "\n">>, Req, DbList}.

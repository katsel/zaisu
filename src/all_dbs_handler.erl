%% @doc Index handler.
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
    Req2 = cowboy_req:set_resp_body(
        <<"{\"error\":\"method_not_allowed\",",
		  "\"reason\":\"Only GET,HEAD allowed\"}\n">>, Req),
    {[<<"GET">>, <<"HEAD">>], Req2, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, all_dbs_to_json},
        {<<"application/json">>, all_dbs_to_json}
    ], Req, State}.


all_dbs_to_json(Req, DbList) ->
    AllDbsAsList = lists:map(
        fun(X) -> {Y} = X, Y end,
        lists:usort(ets:tab2list(DbList))),
    Result = jiffy:encode(AllDbsAsList),
    {<<Result/binary, "\n">>, Req, DbList}.

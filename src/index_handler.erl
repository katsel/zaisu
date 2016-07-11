%%% @doc Index handler.
-module(index_handler).

%%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%%% Custom callbacks
-export([index_to_json/2]).


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
        {<<"text/plain">>, index_to_json},       % JSON content as plaintext
        {<<"application/json">>, index_to_json}  % JSON content as JSON
    ], Req, State}.


index_to_json(Req, State) ->
    Response = jiffy:encode({[{zaisu, <<"Welcome">>}]}),
    {<<Response/binary, "\n">>, Req, State}.



%% @doc Zaisu handler.
-module(toppage_handler).

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Custom callbacks
-export([content_to_json/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, content_to_json}, % JSON content
		{<<"text/plain">>, content_to_json}  % same content declared as text
	], Req, State}.

content_to_json(Req, State) ->
	{<<"{\"zaisu\": \"Welcome\"}\n">>, Req, State}.

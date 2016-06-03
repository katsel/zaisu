-module(toppage_handler).

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).

%% Custom callbacks
-export([hello_to_text/2]).

init(Req, State) ->
	{cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, hello_to_text}
	], Req, State}.

hello_to_text(Req, State) ->
	{<<"{\"zaisu\": \"Welcome\"}">>, Req, State}.

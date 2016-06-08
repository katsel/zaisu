%

%% @private
-module(zaisu_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		%% {HostMatch, list({PathMatch, Handler, Opts})}
		{'_', [
			{"/[:db_name]", toppage_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(my_http_listener, 100, [{port, 8080}],
		#{env => #{dispatch => Dispatch}}
	),
	zaisu_sup:start_link().

stop(_State) ->
	ok.

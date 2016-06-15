%% @doc Database handler.
-module(db_handler).

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([delete_completed/2]).
-export([delete_resource/2]).
-export([is_conflict/2]).
-export([resource_exists/2]).

%% Custom callbacks
-export([content_to_json/2]).
-export([from_generic/2]).

% valid database name
-define(DBNAME_REGEX,
	"^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*"  % use the stock CouchDB regex
	"(\\.[0-9]{10,})?$"  % but allow an optional shard timestamp at the end
).

init(Req, DbList) ->
	{cowboy_rest, Req, DbList}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
	{[
		{'*', from_generic}
	], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, content_to_json},       % JSON content as plaintext
		{<<"application/json">>, content_to_json}  % JSON content as JSON
	], Req, State}.

delete_completed(Req, DbList) ->
	Req2 = cowboy_req:reply(200, #{},
	<<"{\"ok\":true}\n">>, Req),
	{true, Req2, DbList}.

delete_resource(Req, DbList) ->
	DbName = cowboy_req:binding(db_name, Req),
	{delete_database(DbName, DbList), Req, DbList}.

is_conflict(Req, DbList) ->
	DbName = cowboy_req:binding(db_name, Req),
	case db_exists(DbName, DbList) of
		true -> {true, Req, DbList};
		false -> {false, Req, DbList}
	end.

resource_exists(Req, DbList) ->
	DbName = cowboy_req:binding(db_name, Req),
	case validate_dbname(DbName) of
		ok ->
			case db_exists(DbName, DbList) of
				true -> {true, Req, DbList};
				false -> {false, Req, DbList}
			end;
		_ ->
		Req2 = cowboy_req:reply(400, #{},  % 400 Bad Request
			illegal_dbname_warning(DbName), Req),
		{false, Req2, DbList}
	end.


content_to_json(Req, DbList) ->
	DbName = cowboy_req:binding(db_name, Req),
	{<<"{\"db_name\": \"",DbName/binary,"\"}\n">>, Req, DbList}.

from_generic(Req, DbList) ->
	DbName = cowboy_req:binding(db_name, Req),
	create_database(DbName, DbList),
	Req2 = cowboy_req:reply(201, #{},
		<<"{\"ok\":true}\n">>, Req),
	{true, Req2, DbName}.

% Private

create_database(DbName, DbList) ->
	ets:insert_new(DbList, {DbName}).

db_exists(DbName, DbList) ->
	ets:member(DbList, DbName).

delete_database(DbName, DbList) ->
	ets:delete(DbList, DbName).

illegal_dbname_warning(DbName) ->
	<<"{\"error\":\"illegal_database_name\",\"reason\":\"Name: '",
	DbName/binary, "'. ",
	"Only lowercase characters (a-z), digits (0-9), and any of the ",
	"characters _, $, (, ), +, -, and / are allowed. ",
	"Must begin with a letter.\"}\n">>.

validate_dbname(DbName) when is_binary(DbName) ->
	case re:run(DbName, ?DBNAME_REGEX, [{capture,none}, dollar_endonly]) of
		match ->
			ok;
		nomatch ->
			{error, {illegal_database_name, DbName}}
			%% ignore systemdbs for now
	end.

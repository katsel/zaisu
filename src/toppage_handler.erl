

%% @doc Zaisu handler.
-module(toppage_handler).

%% Standard callbacks
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

%% Custom callbacks
-export([content_to_json/2]).

% valid database name
-define(DBNAME_REGEX,
	"^[a-z][a-z0-9\\_\\$()\\+\\-\\/]*"  % use the stock CouchDB regex
	"(\\.[0-9]{10,})?$"  % but allow an optional shard timestamp at the end
).

init(Req, DbList) ->
	{cowboy_rest, Req, DbList}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, content_to_json},       % JSON content as plaintext
		{<<"application/json">>, content_to_json}  % JSON content as JSON
	], Req, State}.

resource_exists(Req, DbList) ->
	case cowboy_req:binding(db_name, Req) of
		undefined ->
			{true, Req, index};
		DbName ->
			case validate_dbname(DbName) of
				ok ->
					case db_exists(DbName, DbList) of
						true -> {true, Req, DbName};
						false -> {false, Req, DbName}
					end;
				_ ->
				Req2 = cowboy_req:reply(400, #{},  % 400 Bad Request
					illegal_dbname_warning(DbName), Req),
				{false, Req2, invalidDbName}
			end
	end.


content_to_json(Req, index) ->
	{<<"{\"zaisu\": \"Welcome\"}\n">>, Req, index};
content_to_json(Req, DbName) ->
	{<<"{\"db_name\": \"",DbName/binary,"\"}\n">>, Req, DbName}.

% Private

db_exists(DbName, DbList) ->
	lists:member(DbName, DbList).

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

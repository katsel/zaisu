{application, zaisu, [
	{description, "CouchDB mock replication endpoint"},
	{vsn, "0.0.1"},
	{modules, ['all_dbs_handler','db_handler','index_handler','zaisu_app','zaisu_sup','zaisu_tests']},
	{registered, [zaisu_sup]},
	{applications, [kernel,stdlib,sasl,cowboy,jiffy]},
	{mod, {zaisu_app, []}}
]}.

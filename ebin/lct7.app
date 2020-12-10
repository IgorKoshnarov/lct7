{application, 'lct7', [
	{description, "Cache http service"},
	{vsn, "0.1.0"},
	{modules, ['lct7_app','lct7_bucket_manager','lct7_cache_server','lct7_cache_server_controller','lct7_json','lct7_sup']},
	{registered, [lct7_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {lct7_app, []}},
	{env, []}
]}.
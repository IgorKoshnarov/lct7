-define(DEFAULT_BUCKET, application:get_env(lct7, default_bucket, <<"default">>)).
-define(DEFAULT_TTL, application:get_env(lct7, default_ttl, 3600)).

-record(insert_params, {bucket = ?DEFAULT_BUCKET, key, value, ttl = ?DEFAULT_TTL}).
-record(lookup_params, {bucket = ?DEFAULT_BUCKET, key}).
-record(lookup_by_date_params, {bucket = ?DEFAULT_BUCKET, date_from, date_to}).
-record(kv, {key, value, timestamp, expire}).
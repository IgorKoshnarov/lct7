-module(lct7_cache_server_SUITE).

-include("../include/lct7_cache.hrl").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

init_per_suite(Config) ->
    gen_server:start({local, lct7_cache_server}, lct7_cache_server, [], []),
    Config.

end_per_suite(_Config) ->
    gen_server:stop(lct7_cache_server),
    ok.

all() -> [
    lct7_cache_server_test
].

lct7_cache_server_test(_Config) ->
    InsertTime = calendar:universal_time(),
    {{Y, Mo, D}, {H, Mi, S}} = InsertTime,
    LaterTime = {{Y, Mo, D}, {H, Mi, S + 3}},
    InsertParams = #insert_params{key = <<"some key">>, value = [1, 2, 3], ttl = 2},
    lct7_cache_server:insert(InsertParams),
    LookupParams = #lookup_params{key = <<"some key">>},
    [1, 2, 3] = lct7_cache_server:lookup(LookupParams),
    LookupByDateParams = #lookup_by_date_params{date_from = InsertTime, date_to = LaterTime},
    [{<<"some key">>,[1,2,3]}] = lct7_cache_server:lookup_by_date(LookupByDateParams),
    timer:sleep(3000),
    undefined = lct7_cache_server:lookup(LookupParams),
    [] = lct7_cache_server:lookup_by_date(LookupByDateParams).
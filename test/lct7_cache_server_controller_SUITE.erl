-module(lct7_cache_server_controller_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).
-compile(nowarn_export_all).

all() -> [
    lct7_test
].

init_per_suite(Config) ->
    inets:start(),
    application:ensure_all_started(lct7),
    Config.

end_per_suite(_Config) ->
    application:stop(lct7),
    inets:stop().

post(Body) ->
    httpc:request(post, {"http://127.0.0.1:8080/api/cache_server", [], "application/json", Body}, [], []).

lct7_test(_Config) ->
    % insert
    InsertTime = calendar:universal_time(),    
    InsertTimestamp = calendar:datetime_to_gregorian_seconds(InsertTime),
    InsertTimeBin = time_to_bin(InsertTime),
    LookupTimeBin = time_to_bin(calendar:gregorian_seconds_to_datetime(InsertTimestamp + 3)),
    InsertResp = post(<<"{\"action\": \"insert\", \"key\": \"some_key\", \"value\": [1, 2, 3]}">>),
    ok = element(1, InsertResp),
    InsertRespData = element(2, InsertResp),
    {_, 200, "OK"} = element(1, InsertRespData),
    "{\"result\":\"ok\"}" = element(3, InsertRespData),
    % lookup
    LookupResp = post(<<"{\"action\": \"lookup\", \"key\": \"some_key\"}">>),
    ok = element(1, LookupResp),
    LookupRespData = element(2, LookupResp),
    {_, 200, "OK"} = element(1, LookupRespData),
    "{\"result\":[1,2,3]}" = element(3, LookupRespData),
    % lookup by date
    LookupByDateResp = 
        post(<<"{\"action\": \"lookup_by_date\", \"date_from\": \"" , 
        InsertTimeBin/binary, "\", \"date_to\": \"", 
        LookupTimeBin/binary, "\"}">>),
    ok = element(1, LookupByDateResp),
    LookupByDateRespData = element(2, LookupByDateResp),
    {_, 200, "OK"} = element(1, LookupByDateRespData),
    "{\"result\":[{\"key\":\"some_key\",\"value\":[1,2,3]}]}" = element(3, LookupByDateRespData).

time_to_bin({{Y, Mo, D}, {H, Mi, S}}) ->
    {YBin, MoBin, DBin, HBin, MiBin, SBin} = 
        {integer_to_binary(Y), integer_to_binary(Mo), integer_to_binary(D), pad(H), pad(Mi), pad(S)},
    <<YBin/binary,"/",MoBin/binary,"/",DBin/binary," ",HBin/binary,":",MiBin/binary,":",SBin/binary>>.

pad(D) when D >= 10 ->
    integer_to_binary(D);
pad(D) when D < 10 ->
    C = 48 + D,
    <<$0,C>>.

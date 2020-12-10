-module(lct7_cache_server_controller).

-export([init/2]).

-export([handle/2]).

-include("../include/lct7_cache.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    Type = cowboy_req:header(<<"content-type">>, Req),
    case {Method, Type} of
        {<<"POST">>, <<"application/json">>} -> 
            {ok, Body, Req1} = read_body(Req, <<>>),
            case lct7_json:decode(Body) of 
                {ok, Result} -> 
                    {ok, handle(Result, Req1), Opts};
                {error, Reason} -> 
                    {ok, bad_request(Reason, Req1), Opts}
            end;
        _ ->
            {ok, bad_request(Req), Opts}
    end.

response(Status, Body, Req) -> 
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, 
    lct7_json:encode(Body), Req).

bad_request(Req) ->
    response(400, #{<<"error">> => <<"Bad request">>}, Req).

bad_request(Error, Req) ->
    response(400, #{<<"error">> => term_to_binary(Error)}, Req).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

handle(Json, Req) ->
    try
        Result = handle(Json),
        response(200, #{<<"result">> => Result}, Req)
    catch
        _Error:Reason -> 
            bad_request(Reason, Req)
    end.

handle(#{
        <<"action">> := <<"insert">>, 
        <<"bucket">> := Bucket, 
        <<"key">> := Key, 
        <<"value">> := Value, 
        <<"ttl">> := Ttl
        }) ->
    lct7_cache_server:insert(#insert_params{bucket = Bucket, key = Key, value = Value, ttl = Ttl});
handle(#{
        <<"action">> := <<"insert">>, 
        <<"bucket">> := Bucket, 
        <<"key">> := Key, 
        <<"value">> := Value
        }) ->
    lct7_cache_server:insert(#insert_params{bucket = Bucket, key = Key, value = Value});
handle(#{
        <<"action">> := <<"insert">>, 
        <<"key">> := Key, 
        <<"value">> := Value,
        <<"ttl">> := Ttl
        }) ->
    lct7_cache_server:insert(#insert_params{key = Key, value = Value, ttl = Ttl});
handle(#{
        <<"action">> := <<"insert">>, 
        <<"key">> := Key, 
        <<"value">> := Value 
        }) ->
    lct7_cache_server:insert(#insert_params{key = Key, value = Value});
handle(#{
        <<"action">> := <<"lookup">>, 
        <<"bucket">> := Bucket,
        <<"key">> := Key
        }) ->
    lct7_cache_server:lookup(#lookup_params{bucket = Bucket, key = Key});    
handle(#{
        <<"action">> := <<"lookup">>, 
        <<"key">> := Key
        }) ->
    lct7_cache_server:lookup(#lookup_params{key = Key});
handle(#{
        <<"action">> := <<"lookup_by_date">>, 
        <<"bucket">> := Bucket,        
        <<"date_from">> := DateFrom, 
        <<"date_to">> := DateTo
        }) ->
    lct7_cache_server:lookup_by_date(
        #lookup_by_date_params{bucket = Bucket, date_from = bin_to_date(DateFrom), date_to = bin_to_date(DateTo)}
    );
handle(#{
        <<"action">> := <<"lookup_by_date">>, 
        <<"date_from">> := DateFrom, 
        <<"date_to">> := DateTo
        }) ->
    lct7_cache_server:lookup_by_date(
        #lookup_by_date_params{date_from = bin_to_date(DateFrom), date_to = bin_to_date(DateTo)}
    ).

bin_to_date(<<Y,E,A,R,"/",M,O,"/",D,T," ",H,U,":",N,L,":",S,C>>) ->
    [Year, Month, Date, Hour, Minute, Second] = 
        [element(1, string:to_integer(X)) || X <- [<<Y,E,A,R>>, <<M,O>>, <<D,T>>, <<H,U>>, <<N,L>>, <<S,C>>]],
    {{Year, Month, Date}, {Hour, Minute, Second}};
bin_to_date(<<Y,E,A,R,"/",M,"/",D,T," ",H,U,":",N,L,":",S,C>>) ->
    [Year, Month, Date, Hour, Minute, Second] = 
        [element(1, string:to_integer(X)) || X <- [<<Y,E,A,R>>, <<M>>, <<D,T>>, <<H,U>>, <<N,L>>, <<S,C>>]],
    {{Year, Month, Date}, {Hour, Minute, Second}};
bin_to_date(<<Y,E,A,R,"/",M,O,"/",D," ",H,U,":",N,L,":",S,C>>) ->
    [Year, Month, Date, Hour, Minute, Second] = 
        [element(1, string:to_integer(X)) || X <- [<<Y,E,A,R>>, <<M,O>>, <<D>>, <<H,U>>, <<N,L>>, <<S,C>>]],
    {{Year, Month, Date}, {Hour, Minute, Second}};
bin_to_date(<<Y,E,A,R,"/",M,"/",D," ",H,U,":",N,L,":",S,C>>) ->
    [Year, Month, Date, Hour, Minute, Second] = 
        [element(1, string:to_integer(X)) || X <- [<<Y,E,A,R>>, <<M>>, <<D>>, <<H,U>>, <<N,L>>, <<S,C>>]],
    {{Year, Month, Date}, {Hour, Minute, Second}}.

-ifdef(TEST).

bin_to_date_test_() -> [
    ?_assertEqual({{2015,2,1},{22,1,3}}, bin_to_date(<<"2015/2/1 22:01:03">>))
].

-endif.

-module(lct7_bucket_manager).

-behaviour(gen_server).

-include("../include/lct7_cache.hrl").

-define(DROP_INTERVAL, application:get_env(lct7, drop_interval, 3600_000)). % timer accepts milliseconds
-define(NOW, calendar:datetime_to_gregorian_seconds(calendar:universal_time())).

-export([insert/2, lookup/2, lookup_by_date/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link(_Bucket) ->
    gen_server:start_link(?MODULE, [], []).

init(_Bucket) ->
    timer:send_after(?DROP_INTERVAL, self(), clean),
    {ok, ets:new(table, [{keypos, #kv.key}])}.

insert(Pid, #insert_params{key = Key, value = Value, ttl = Ttl}) ->
    gen_server:cast(Pid, {insert, Key, Value, Ttl}).

lookup(Pid, #lookup_params{key = Key}) ->
    gen_server:call(Pid, {lookup, Key}).

lookup_by_date(Pid, #lookup_by_date_params{date_from = DateFrom, date_to = DateTo}) ->
    gen_server:call(Pid, {lookup_by_date, DateFrom, DateTo}).

handle_cast({insert, Key, Value, Ttl}, Table) ->
    Expire = ?NOW + Ttl,
    ets:insert(Table, #kv{key = Key, value = Value, timestamp = ?NOW, expire = Expire}),
    {noreply, Table}.

handle_call({lookup, Key}, _, Table) ->
    case ets:lookup(Table, Key) of
        [Result] -> 
            {reply, not_expired(Result), Table};
        _ -> 
            {reply, undefined, Table}
    end;
handle_call({lookup_by_date, DateFrom, DateTo}, _, Table) ->
    Now = ?NOW,
    From = calendar:datetime_to_gregorian_seconds(DateFrom),
    To = calendar:datetime_to_gregorian_seconds(DateTo),
    Result = ets:select(Table, [
        {#kv{key = '$1', value = '$2', expire = '$3', timestamp = '$4'}, 
        [{'>=', '$3', Now}, {'>=', '$4', From}, {'=<', '$4', To}], 
        [{{'$1', '$2'}}]}
    ]),
    {reply, Result, Table}.

handle_info(clean, Table) ->
    Now = ?NOW,
    ets:select_delete(Table, [{#kv{expire = '$1', _ = '_'}, [{'<', '$1', Now}], [true]}]),
    timer:send_after(?DROP_INTERVAL, self(), clean),
    {noreply, Table}.

terminate(normal, _State) -> 
    ok.

code_change(_Old, State, _Extra) -> 
    {ok, State}.

not_expired(#kv{value = Value, expire = Expire}) ->
    case ?NOW =< Expire of
        true -> Value;
        _ -> undefined
    end.


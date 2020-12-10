-module(lct7_cache_server).

-behaviour(gen_server).

-include("../include/lct7_cache.hrl").

-export([insert/1, lookup/1, lookup_by_date/1]).
-export([start_link/0, init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Opts) ->
    ets:new(buckets, [named_table]),
    process_flag(trap_exit, true),
    {ok, []}.

insert(Params) ->
    gen_server:cast(?MODULE, {insert, Params}).

lookup(Params) ->
    gen_server:call(?MODULE, {lookup, Params}).

lookup_by_date(Params) ->
    gen_server:call(?MODULE, {lookup_by_date, Params}).

handle_info({'EXIT', Pid, _Reason}, State) ->
    ets:select_delete(buckets, [{{'$1', '$2'}, [{'==', '$2', Pid}], [true]}]),
    {noreply, State}.

handle_cast({insert, #insert_params{bucket = Bucket} = Params}, State) ->
    case ets:lookup(buckets, Bucket) of
        [{Bucket, Pid}] -> 
            lct7_bucket_manager:insert(Pid, Params),
            {noreply, State};
        _ -> 
            Pid = new_bucket(Bucket),
            lct7_bucket_manager:insert(Pid, Params),
            {noreply, State}
    end.

handle_call({lookup, #lookup_params{bucket = Bucket} = Params}, _, State) ->
    case ets:lookup(buckets, Bucket) of
        [{Bucket, Pid}] -> 
            {reply, lct7_bucket_manager:lookup(Pid, Params), State};
        _ -> 
            Pid = new_bucket(Bucket),
            {reply, lct7_bucket_manager:lookup(Pid, Params), State}
    end;
handle_call({lookup_by_date, #lookup_by_date_params{bucket = Bucket} = Params}, _, State) ->
    case ets:lookup(buckets, Bucket) of
        [{Bucket, Pid}] -> 
            {reply, lct7_bucket_manager:lookup_by_date(Pid, Params), State};
        _ -> 
            Pid = new_bucket(Bucket),
            {reply, lct7_bucket_manager:lookup_by_date(Pid, Params), State}
    end.

terminate(normal, _State) -> 
    ok;
terminate(shutdown, _State) -> 
    ok.

code_change(_Old, State, _Extra) -> 
    {ok, State}.

new_bucket(Bucket) ->
    {ok, Pid} = gen_server:start_link(lct7_bucket_manager, Bucket, []),
    true = ets:insert(buckets, {Bucket, Pid}),
    Pid.

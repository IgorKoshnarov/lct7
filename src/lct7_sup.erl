-module(lct7_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
	ChildSpecs = [
        #{
            id => lct7_cache_server,
            start => {lct7_cache_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [lct7_cache_server]
        }		
	],
	{ok, {SupFlags, ChildSpecs}}.

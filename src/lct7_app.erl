-module(lct7_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/api/cache_server", lct7_cache_server_controller, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	lct7_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).

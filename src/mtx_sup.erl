-module(mtx_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).
-define(CHILD(I, Type, Opts), {I, {I, start_link, [Opts]}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, get_hostport()).
init([Host, Port]) -> {ok, { {one_for_one, 5, 10}, [?CHILD(mtx, worker, [Host, Port])]} }.

get_hostport() ->
    {ok, IP} = application:get_env(mtx, ip),
    {ok , Port} = application:get_env(mtx, port),
    [IP, Port].

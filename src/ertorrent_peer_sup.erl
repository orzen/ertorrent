-module(ertorrent_peer_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ertorrent_peer_sup}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {simple_one_for_one, 4, 5},
    ChildSpec = {ertorrent_peer_worker,
                 {ertorrent_peer_worker, start_link, []},
                 transient, 60*1000, worker, [ertorrent_peer_worker]},
    {ok, {SupFlags, [ChildSpec]}}.

-module(ertorrent_peer_ssup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ertorrent_peer_ssup}, ?MODULE, []).

init(_Arg) ->
    MaxRestart = 4,
    MaxTime = 5,

    SupFlags = {one_for_one, MaxRestart, MaxTime},

    {ok, Listen_socket} = gen_tcp:listen(?PORT, [{active, once}, {packet, line}]),

    Peer_sup_specs = {ertorrent_peer_sup,
                      {ertorrent_peer_sup, start_link, []},
                      transient, infinity, supervisor, [ertorrent_peer_sup]},

    Peer_srv_specs = {ertorrent_peer_srv,
                      {ertorrent_peer_srv, start_link, [[37555]]},
                      transient, 60*1000, worker, [ertorrent_peer_srv]},

    {ok, {SupFlags,[SupSpecs, SrvSpecs]}}.

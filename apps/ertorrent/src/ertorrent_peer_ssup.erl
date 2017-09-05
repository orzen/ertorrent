-module(ertorrent_peer_ssup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ertorrent_peer_ssup}, ?MODULE, []).

init(_Arg) ->
    MaxRestart = 4,
    MaxTime = 5,

    Sup_flags = {one_for_one, MaxRestart, MaxTime},

    {ok, Listen_socket} = gen_tcp:listen(37557, [{active, once}, {packet, line}]),

    Peer_in_specs = {ertorrent_peer_accept,
                     {ertorrent_peer_accept, start_link, [Listen_socket]},
                     transient, 60*1000, worker, [ertorrent_peer_accept]},

    Peer_sup_specs = {ertorrent_peer_sup,
                      {ertorrent_peer_sup, start_link, []},
                      transient, infinity, supervisor, [ertorrent_peer_sup]},

    Peer_srv_specs = {ertorrent_peer_srv,
                      {ertorrent_peer_srv, start_link, [[37555]]},
                      transient, 60*1000, worker, [ertorrent_peer_srv]},

    {ok, {Sup_flags, [Peer_in_specs, Peer_sup_specs, Peer_srv_specs]}}.

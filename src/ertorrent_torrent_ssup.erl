-module(ertorrent_torrent_ssup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ertorrent_torrent_ssup}, ?MODULE, []).

init(_Arg) ->
    MaxRestart = 4,
    MaxTime = 5,

    SupFlags = {one_for_one, MaxRestart, MaxTime},

    SupSpecs = {ertorrent_torrent_sup,
                {ertorrent_torrent_sup, start_link, []},
                transient, infinity, supervisor, [ertorrent_torrent_sup]},

    SrvSpecs = {ertorrent_torrent_srv,
                {ertorrent_torrent_srv, start_link, [[37555]]},
                transient, 60*1000, worker, [ertorrent_torrent_srv]},

    {ok, {SupFlags,[SupSpecs, SrvSpecs]}}.

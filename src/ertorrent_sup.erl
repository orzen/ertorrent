-module(ertorrent_sup).

-define(TRACKER, ertorrent_tracker_sup).
-define(TORRENT, ertorrent_torrent_ssup).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    MaxRestart = 4,
    MaxTime = 5,

    SupFlags = {one_for_one, MaxRestart, MaxTime},

    TrackerSpecs = {?TRACKER,
                    {?TRACKER, start_link, []},
                    transient, infinity, supervisor, [?TRACKER]},

    TorrentSpecs = {?TORRENT,
                    {?TORRENT, start_link, []},
                    transient, infinity, supervisor, [?TORRENT]},

    {ok, {SupFlags, [TrackerSpecs, TorrentSpecs]}}.

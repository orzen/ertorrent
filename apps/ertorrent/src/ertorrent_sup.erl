-module(ertorrent_sup).

-define(HASH, ertorrent_hash_sup).
-define(TRACKER, ertorrent_tracker_ssup).
-define(PEER, ertorrent_peer_ssup).
-define(TORRENT, ertorrent_torrent_ssup).
-define(SETTINGS, ertorrent_settings_sup).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 0,
    Max_time = 5,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Settings_specs = {?SETTINGS,
                      {?SETTINGS, start_link, []},
                      transient, infinity, supervisor, [?SETTINGS]},

    Hash_specs = {?HASH,
                  {?HASH, start_link, []},
                  transient, infinity, supervisor, [?HASH]},

    Peer_specs = {?PEER,
                  {?PEER, start_link, []},
                  transient, infinity, supervisor, [?PEER]},

    Tracker_specs = {?TRACKER,
                     {?TRACKER, start_link, []},
                     transient, infinity, supervisor, [?TRACKER]},

    Torrent_specs = {?TORRENT,
                     {?TORRENT, start_link, []},
                     transient, infinity, supervisor, [?TORRENT]},

    {ok, {Sup_flags, [Settings_specs,
                      Hash_specs,
                      Peer_specs,
                      Tracker_specs,
                      Torrent_specs]}}.

-module(ertorrent_sup).

-define(HASH, ertorrent_hash_sup).
-define(TRACKER, ertorrent_tracker_sup).
-define(TORRENT, ertorrent_torrent_ssup).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 4,
    Max_time = 5,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Hash_specs = {?HASH,
                  {?HASH, start_link, []},
                  transient, infinity, supervisor, [?HASH]},

    Tracker_specs = {?TRACKER,
                     {?TRACKER, start_link, []},
                     transient, infinity, supervisor, [?TRACKER]},

    Torrent_specs = {?TORRENT,
                     {?TORRENT, start_link, []},
                     transient, infinity, supervisor, [?TORRENT]},

    {ok, {Sup_flags, [Hash_specs,
                      Tracker_specs,
                      Torrent_specs]}}.

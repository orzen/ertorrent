-module(ertorrent_sup).

-define(HASH, ertorrent_hash_sup).
-define(TRACKER, ertorrent_tracker_ssup).
-define(PEER, ertorrent_peer_ssup).
-define(TORRENT, ertorrent_torrent_ssup).

-export([start_link/0,
         init/1]).

%%% boot_one should contain configuration handling, OS and
%%% distributed-specifics
boot_one() ->
    ok.

%%% boot_two should contain the majority of the internal functionality
boot_two() ->
    Hash_specs = {?HASH,
                  {?HASH, start_link, []},
                  transient, infinity, supervisor, [?HASH]},

    Tracker_specs = {?TRACKER,
                     {?TRACKER, start_link, []},
                     transient, infinity, supervisor, [?TRACKER]},

    Torrent_specs = {?TORRENT,
                     {?TORRENT, start_link, []},
                     transient, infinity, supervisor, [?TORRENT]},

    Specs = [Hash_specs,
             Tracker_specs,
             Torrent_specs],

    lists:foreach(
        fun(X) ->
            supervisor:start_child(?MODULE, X),
        end,
        Specs
    ),

%%% boot_three should contain the web interface
boot_three() ->
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 0,
    Max_time = 5,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Settings_specs = {?SETTINGS,
                      {?SETTINGS, start_link, []},
                      transient, infinity, supervisor, [?SETTINGS]},

    {ok, {Sup_flags, [Settings_specs]}}.

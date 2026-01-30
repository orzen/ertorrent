-module(ertorrent_torrent_worker_lib).

-export([startup/0]).


read_files() ->
    ok.

hash_files() ->
    ok.

determine_tracker_conf() ->
    ok.

connect_tracker() ->
    ok.

startup() ->
    read_files(),
    hash_files(),
    determine_tracker_conf(),
    connect_tracker().

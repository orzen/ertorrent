-module(ertorrent_file_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(FILE_WORKER, ertorrent_file_worker).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 10,
    Max_time = 10,

    Sup_flags = {simple_one_for_one, Max_restart, Max_time},

    File_w_specs = {?FILE_WORKER,
                    {?FILE_WORKER, start_link, []},
                    transient, 60*1000, worker, [?FILE_WORKER]},

    {ok, {Sup_flags, [File_w_specs]}}.

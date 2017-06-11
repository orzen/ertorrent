-module(ertorrent_file_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(FILE_SRV, ertorrent_file_srv).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 4,
    Max_time = 5,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    File_srv_specs = {?FILE_SRV,
                      {?FILE_SRV, start_link, []},
                      transient, 60*1000, worker, [?FILE_SRV]},

    {ok, {Sup_flags, [File_srv_specs]}}.

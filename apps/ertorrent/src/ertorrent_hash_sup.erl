-module(ertorrent_hash_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(HASH_SRV, ertorrent_hash_srv).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 4,
    Max_time = 5,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Hash_srv_specs = {?HASH_SRV,
                      {?HASH_SRV, start_link, []},
                      transient, 60*1000, worker, [?HASH_SRV]},

    {ok, {Sup_flags, [Hash_srv_specs]}}.

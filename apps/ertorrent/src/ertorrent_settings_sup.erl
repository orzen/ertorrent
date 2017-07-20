-module(ertorrent_settings_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ertorrent_settings_sup}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {simple_one_for_one, 4, 5},

    Settings_srv_specs = {ertorrent_settings_worker,
                         {ertorrent_settings_worker, start_link, []},
                         transient, 60*1000, worker, [ertorrent_settings_worker]},

    {ok, {SupFlags, [Settings_srv_specs]}}.

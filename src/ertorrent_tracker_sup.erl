-module(ertorrent_tracker_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(HTTP_DISPATCHER, ertorrent_tracker_http_dispatcher).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    MaxRestart = 4,
    MaxTime = 5,

    SupFlags = {one_for_one, MaxRestart, MaxTime},

    DispatcherSpecs = {?HTTP_DISPATCHER,
                       {?HTTP_DISPATCHER, start_link, []},
                       transient, 60*1000, worker, [?HTTP_DISPATCHER]},

    {ok, {SupFlags,[DispatcherSpecs]}}.

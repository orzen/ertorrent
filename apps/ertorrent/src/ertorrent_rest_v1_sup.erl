-module(ertorrent_rest_v1_sup).
-behaviour(supervisor).

-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(REST_V1_TOP, ertorrent_rest_v1_top).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Rest_v1_top_specs = {?REST_V1_TOP,
                         {?REST_V1_TOP, start_link, []},
                         transient, infinity, worker, [?REST_V1_TOP]},
    {ok, {{one_for_one, 10, 10}, [Rest_v1_top_specs]}}.

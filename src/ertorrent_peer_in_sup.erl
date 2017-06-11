-module(ertorrent_peer_tcp_sup).

-behaviour(supervisor).

-export([start_link/0,
         init/1]).

-define(CHILD, ertorrent_peer_tcp_worker).
-define(PORT, 44444).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 4,
    Max_time = 5,

    {ok, Listen_socket} = gen_tcp:listen(?PORT, [{active, once}, {packet, line}]),

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Child_specs = {?CHILD,
                   {?CHILD, start_link, [Listen_socket]},
                   transient, 60*1000, worker, [?CHILD]},

    {ok, {Sup_flags, [Child_specs]}}.

start_worker(Listen_socket) ->
    supervisor:start_child(?MODULE, [Listen_socket]).

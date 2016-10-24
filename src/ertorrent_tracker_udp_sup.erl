-module(ertorrent_tracker_udp_sup).

-behaviour(supervisor).

-export([
         start_child/9,
         terminate_child/1
        ]).

-export([
         start_link/0,
         init/1
        ]).

start_child( ) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),
    case gen_udp:open(Src_port, [Version]) of
        {ok, Socket} ->
            supervisor:start_child(?MODULE, [self(), ]).
            
        {error, Reason} ->
            lager:error("failed to open UDP socket for tracker communication: '~w'", [Reason]),
            {error, Reason}
    end.

terminate_child(Peer_id) ->
    case supervisor:terminate(?MODULE, Peer_id) of
        ok -> ok;
        {error, Reason} ->
            lager:error("~p:~p failed to terminate tracker worker '~p'",
                        [?MODULE, ?FUNCTION_NAME, Reason]),
            error
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {simple_one_for_one, 1, 1},

    Peer_worker_specs = {ertorrent_tracker_udp_worker,
                         {ertorrent_tracker_udp_worker, start_link, []},
                         transient, 60*1000, worker, [ertorrent_tracker_udp_worker]},

    {ok, {SupFlags, [Peer_worker_specs]}}.

-module(ertorrent_peer_sup).

-behaviour(supervisor).

-export([
         start_child/9,
         terminate_child/1
        ]).

-export([
         start_link/0,
         init/1
        ]).

-define(SETTINGS, ertorrent_settings_srv).

%-type start_child(Peer_id::atom(), Block_length::integer(),
%                  Mode::atom(), Info_hash, Client_id,
%                  Piece_length::integer(), Socket, Torrent_pid::atom(),
%                  Peer_statem_pid::atom()) -> startchild_ret().
start_child(Peer_id, Block_length, Mode, Info_hash, Client_id, Piece_length,
            Socket, Torrent_pid, Peer_statem_pid) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),
    supervisor:start_child(?MODULE, [Peer_id, Block_length, Mode, Info_hash,
                                     Client_id, Piece_length, Socket,
                                     Torrent_pid, Peer_statem_pid]).

terminate_child(Peer_id) ->
    case supervisor:terminate(?MODULE, Peer_id) of
        ok -> ok;
        {error, Reason} ->
            lager:error("~p:~p failed to terminate peer '~p'",
                        [?MODULE, ?FUNCTION_NAME, Reason]),
            error
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {simple_one_for_one, 1, 1},

    Peer_worker_specs = {ertorrent_peer_worker,
                         {ertorrent_peer_worker, start_link, []},
                         transient, 60*1000, worker, [ertorrent_peer_worker]},

    {ok, {SupFlags, [Peer_worker_specs]}}.

%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc peer_srv.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_peer_srv).

-behaviour(gen_server).

-export([start_link/1,
         stop/0,
         add_rx_peer/3,
         add_tx_peer/3,
         list/1,
         queue/2,
         remove/1,
         remove_related/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(SERVER, ?MODULE).
-define(SUPERVISOR, ertorrent_peer_sup).
-define(SSUPERVISOR, ertorrent_peer_ssup).
-define(TORRENT_SRV, ertorrent_torrent_srv).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {peers=[],
                peer_id,
                port_max,
                port_min,}).

%%% Client API
add_rx_peer(Socket, Info_hash, Peer_id) ->
    gen_server:cast(?MODULE, {peer_srv_add_peer_rx, Socket, Info_hash, Peer_id}).

add_tx_peer(Address, Info_hash, Peer_id) ->
    gen_server:cast(?MODULE, {peer_srv_add_peer_tx, Address, Info_hash, Peer_id}).

list(Pid) ->
    gen_server:cast(Pid, {peer_srv_list_peers}).

print_list([]) ->
    true;
print_list([H|T]) ->
    {Hash, _} = H,
    io:format("~p~n", [Hash]),
    print_list(T).

remove(Address) ->
    gen_server:call(?MODULE, {remove, Address}).

remove_related(Info_hash) ->
    gen_server:call(?MODULE, {remove_related, Info_hash}).

%%% Standard client API
start_link([Port_min, Port_max, Peer_id]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Callback module
init([ID, Port_min, Port_max, Peer_id]) ->
    {ok, #state{id=ID, peer_id=Peer_id, port_min=Port_min, port_max=Port_max}}.

%% Synchronous
handle_call(_Req, _From, State) ->
    {noreply, State}.

%% Asynchronous
handle_call({start}, _From, _State) ->
    io:format("~p starting~n",[?MODULE]),
    {ok};

handle_call({remove_peer}, _From, _State) ->
    io:format("~p remove~n",[?MODULE]),
    {ok};
handle_call({list_peers}, _From, State) ->
    io:format("~p list~n",[?MODULE]),

    Torrents = State#state.torrents,

    print_list(Torrents),

    {reply, ok, State}.

handle_cast({peer_srv_add_peer_in, Socket, Info_hash, Peer_id}, State) ->
    % Validate the Info_hash
    case ?TORRENT_SRV:member_by_info_hash(Info_hash) of
        true ->
            Ret = supervisor:start_child(?SUPERVISOR,
                                         [Atom_hash,
                                             [ID,
                                              Info_hash,
                                              State#state.peer_id,
                                              Socket]]);
        false ->
            ?WARNING("incoming peer request for a non-existing torrent with hash:" ++ Info_hash )
    end,

handle_cast({peer_srv_add_peer_out, {Address, Port, Info_hash, Peer_id, Socket, Torrent_pid}}, State)) ->
    Port_str = integer_to_list(Port),

    % Building an unique ID for the peer since the peer_srv is shared by all
    % the torrent workers. It cannot be ruled out that the same address won't
    % occur twice by different torrents. Therefore their IDs have to be
    % destinguishable in order to manage them seperately.
    ID_str = Address ++ Port ++ Info_hash,
    ID_hash = crypto:hash(sha, Peer_info_hash),
    ID_atom = binary_to_atom(ID_hash),

    case gen_tcp:connect(Address, Port, [binary, {packet, 0}]) of
        {ok, Socket} ->
            Ret = supervisor:start_child(?SUPERVISOR,
                                         [Atom_hash,
                                             [ID_atom,
                                              Info_hash,
                                              State#state.peer_id,
                                              Socket]]),
            case Ret of
                % TODO Atm the handling of the {ok, _} responses is redundant.
                % It's unlikely that both will be used however in the writing
                % moment this cannot be determined and therefore both are taken
                % into consideration until one could be ruled out.
                {ok, Peer_pid} ->
                    Peers = State#state.peers,

                    ok = gen_server:cast(Peer_pid, transmit),

                    New_state = State#state{peers=[{ID_atom,
                                                    Peer_pid,
                                                    Torrent_pid,
                                                    Address, Port,
                                                    Info_hash}| Peers]},

                    {noreply, New_state};
                {ok, Peer_pid, Info} ->
                    INFO("recv unhandled data 'Info': " ++ Info)

                    Peers = State#state.peers,

                    ok = gen_server:cast(Peer_pid, transmit),

                    New_state = State#state{peers=[{ID_atom,
                                                    Peer_pid,
                                                    Torrent_pid,
                                                    Address, Port,
                                                    Info_hash}| Peers]},

                    {noreply, New_state};
                {error, Reason} ->
                    ERROR("peer_srv failed to spawn a peer_worker, check the peer_sup. reason: " ++ Reason),

                    % TODO if there's an issue with the peer_sup being
                    % unresponsive. An alternative could be to message the
                    % peer_ssup to restart the sup. For now it will only be
                    % logged.

                    {noreply, State}
            end,
        {error, Reason} ->
            DEBUG("peer_srv failed to establish connection with peer: " ++
                  Address ++ ":" ++ Port ++ ", reason: " ++ Reason),

            State#state.torrent_pid ! {peer_srv, requesting_peer},

            {noreply, State}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.

%% Do work here
handle_info({peer_w_read_piece, From, Info_hash, Piece_idx}, State) ->
    ?TORRENT_SRV ! {peer_s_read_piece, From, Info_hash, Piece_idx},
    {noreply, State};
handle_info({peer_w_write_piece, From, Info_hash, Piece_data}, State) ->
    ?TORRENT_SRV ! {peer_s_read_piece, From, Info_hash, Piece_data},
    {noreply, State};
handle_info({peer_w, terminate, Address})
handle_info({'EXIT', _ParentPid, shutdown}, State) ->
    {stop, shutdown, State};
handle_info(_Info, _State) ->
    ok.

terminate(Reason, _State) ->
    io:format("~p: going down, Reason: ~p~n", [?MODULE, Reason]),
    error_logger:info_msg("~p: terminating, reason: ~p~n", [?MODULE, Reason]),
    {ok, State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc peer_srv.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_peer_srv).

-behaviour(gen_server).

-export([start_link/1,
         stop/0,
         add_rx_peer/2,
         add_tx_peer/2,
         remove/1,
         remove_related/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(SETTINGS_SRV, ertorrent_settings_srv).
-define(PEER_SUP, ertorrent_peer_sup).
-define(PEER_SSUP, ertorrent_peer_ssup).
-define(TORRENT_SRV, ertorrent_torrent_srv).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {peers=[],
                own_peer_id,
                port_max,
                port_min}).

%%% Client API
add_rx_peer(Socket, Info_hash) ->
    gen_server:cast(?MODULE, {peer_srv_add_peer_rx, Socket, Info_hash}).

add_tx_peer(Address, Info_hash) ->
    gen_server:cast(?MODULE, {peer_srv_add_peer_tx, Address, Info_hash}).

remove(Address) ->
    gen_server:call(?MODULE, {remove, Address}).

remove_related(Info_hash) ->
    gen_server:call(?MODULE, {remove_related, Info_hash}).

%%% Standard client API
start_link([Port_min, Port_max]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port_min, Port_max], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Callback module
init([Port_min, Port_max]) ->
    Own_peer_id = ?SETTINGS_SRV:call({get, peer_id}),
    {ok, #state{own_peer_id = Own_peer_id, port_min=Port_min, port_max=Port_max}}.

handle_call(Req, From, State) ->
    ?INFO("unhandled call request: " ++ Req ++ ", from: " ++ From),
    {noreply, State}.

handle_cast({peer_srv_add_peer_in, Socket, Info_hash}, State) ->
    ID = erlang:unique_integer(),

    % Retreive the address and port from the socket
    case inet:peername(Socket) of
        {ok, {S_address, S_port}} ->
            Address = S_address,
            Port = S_port;
        {error, Reason_peername} ->
            ?WARNING("failed to retreive address and port form peer_tx socket: "
                     ++ Reason_peername),

            % Set fail-over values for Address and Port, these values are
            % mainly for information so should not be vital.
            Address = unknown,
            Port = unknown
    end,

    Peers = State#state.peers,

    % Validate the Info_hash
    case ?TORRENT_SRV:member_by_info_hash(Info_hash) of
        true ->
            case supervisor:start_child(?PEER_SUP,
                                         [ID,
                                          [ID,
                                           Info_hash,
                                           State#state.own_peer_id,
                                           Socket]]) of
                {ok, Peer_pid} ->
                    New_state = State#state{peers=[{ID,
                                                    Peer_pid,
                                                    Address,
                                                    Port,
                                                    Info_hash}| Peers]};
                {ok, Peer_pid, _Info} ->
                    New_state = State#state{peers=[{ID,
                                                    Peer_pid,
                                                    Address,
                                                    Port,
                                                    Info_hash}| Peers]};
                {error, Reason_sup} ->
                    ?ERROR("peer_srv failed to spawn a peer_worker (rx), check the peer_sup. reason: "
                           ++ Reason_sup),

                    % TODO if there's an issue with the peer_sup being
                    % unresponsive. An alternative could be to message the
                    % peer_ssup to restart the sup. For now it will only be
                    % logged.

                    New_state = State
            end;
        false ->
            ?WARNING("incoming peer request for a non-existing torrent with hash:"
                     ++ Info_hash),

            New_state = State
    end,
    {noreply, New_state};

handle_cast({peer_srv_add_peer_out, {Address, Port, Info_hash, Socket}}, State) ->
    ID = erlang:unique_integer(),

    case gen_tcp:connect(Address, Port, [binary, {packet, 0}]) of
        {ok, Socket} ->
            Ret = supervisor:start_child(?PEER_SUP,
                                         [ID,
                                             [ID,
                                              Info_hash,
                                              State#state.own_peer_id,
                                              Socket]]),
            case Ret of
                % TODO Atm the handling of the {ok, _} responses is redundant.
                % It's unlikely that both will be used however in the writing
                % moment this cannot be determined and therefore both are taken
                % into consideration until one could be ruled out.
                {ok, Peer_pid} ->
                    Peers = State#state.peers,

                    ok = gen_server:cast(Peer_pid, transmit),

                    New_state = State#state{peers=[{ID,
                                                    Peer_pid,
                                                    Address,
                                                    Port,
                                                    Info_hash}| Peers]};
                {ok, Peer_pid, Info} ->
                    ?INFO("recv unhandled data 'Info': " ++ Info),

                    Peers = State#state.peers,

                    ok = gen_server:cast(Peer_pid, transmit),

                    New_state = State#state{peers=[{ID,
                                                    Peer_pid,
                                                    Address,
                                                    Port,
                                                    Info_hash}| Peers]};

                {error, Reason_sup} ->
                    ?ERROR("peer_srv failed to spawn a peer_worker (tx), check the peer_sup. reason: "
                           ++ Reason_sup),

                    % TODO if there's an issue with the peer_sup being
                    % unresponsive. An alternative could be to message the
                    % peer_ssup to restart the sup. For now it will only be
                    % logged.

                    New_state = State
            end,

            {noreply, New_state};
        {error, Reason_connect} ->
            ?INFO("peer_srv failed to establish connection with peer: " ++
                  Address ++ ":" ++ Port ++ ", reason: " ++ Reason_connect),

            ?TORRENT_SRV ! {peer_srv, requesting_peer},

            {noreply, State}
    end;
handle_cast(stop, State) ->
    {stop, normal, State}.

% Forwarded messages
handle_info({peer_w_read_piece, From, Info_hash, Piece_idx}, State) ->
    ?TORRENT_SRV ! {peer_s_read_piece, From, Info_hash, Piece_idx},
    {noreply, State};
handle_info({peer_w_write_piece, From, Info_hash, Piece_data}, State) ->
    ?TORRENT_SRV ! {peer_s_read_piece, From, Info_hash, Piece_data},
    {noreply, State};
handle_info({'EXIT', _ParentPid, shutdown}, State) ->
    {stop, shutdown, State};
handle_info(_Info, _State) ->
    ok.

terminate(Reason, State) ->
    io:format("~p: going down, Reason: ~p~n", [?MODULE, Reason]),
    error_logger:info_msg("~p: terminating, reason: ~p~n", [?MODULE, Reason]),
    {ok, State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

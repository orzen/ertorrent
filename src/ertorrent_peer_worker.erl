%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%% TODO:
%%%-------------------------------------------------------------------

-module(ertorrent_peer_worker).

-behaviour(gen_server).

-include("ertorrent_log.h").
-include("ertorrent_peer_protocol.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([reset_rx_keep_alive/1,
         reset_tx_keep_alive/1,
         send_keep_alive/1
         start_transmit/0]).

-export([start/0,
         start_link/2,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(incoming, {piece_blocks=[],
                   piece_hash,
                   piece_index,
                   piece_downloaded_size,
                   piece_total_size,
                   % Piece queue is a list containing tuples with information for each piece
                   % {index, hash, piece_size}
                   piece_queue=[]}).

-record(outgoing, {outgoing_piece_data=<<>>,
                   outgoing_piece_hash}).

-record(peer_state, {choked,
                     interested}).

-record(state, {id, % peer_worker_id
                % Timer reference for when keep alive is expected from peer
                keep_alive_rx_ref,
                % Timer reference for when keep alive message should be sent
                keep_alive_tx_ref,
                peer_bitfield,
                peer_srv_pid,
                peer_state,
                received_handshake=false,
                request_queue=[],
                socket,
                torrent_bitfield,
                torrent_info_hash,
                torrent_peer_id,
                torrent_pid}).

% 16KB seems to be an inofficial standard among most torrent client
% implementations. Link to the discussion:
% https://wiki.theory.org/Talk:BitTorrentSpecification#Messages:_request
-define(BLOCK_SIZE, 16000).
% Can't make the same assumption for external peers so this should be two
% minutes.
-define(KEEP_ALIVE_RX_TIMER, 120000).
% Should send keep-alive within two minutes. A little less than two minutes
% should be fine.
-define(KEEP_ALIVE_TX_TIMER, 100000).
-define(PEER_SRV, ertorrent_peer_srv).

%%% Extended client API
start_transmit() ->
    gen_server:cast(ID, transmit).

%%% Standard client API

% ID should be the address converted into an atom
start(ID, Info_hash, Peer_id, Socket, Torrent_pid) when is_atom(ID) ->
    gen_server:start({local, ID}, ?MODULE, [ID, Info_hash, Peer_id, Socket, Torrent_pid], []).

start_link(ID, Info_hash, Peer_id, Socket, Torrent_pid) when is_atom(ID) ->
    gen_server:start_link({local, ID}, ?MODULE, [ID, Info_hash, Peer_id, Socket, Torrent_pid], []).

stop(ID) ->
    io:format("stopping~n"),
    gen_server:cast(?MODULE, stop).

%%% Internal functions
reset_rx_keep_alive(Keep_alive_rx_ref) ->
    erlang:cancel(Keep_alive_rx_ref),

    erlang:send_after(?KEEP_ALIVE_RX_TIMER, self(), {keep_alive_rx_timeout}).

reset_tx_keep_alive(Keep_alive_tx_ref) ->
    erlang:cancel(Keep_alive_tx_ref),

    erlang:send_after(?KEEP_ALIVE_TX_TIMER, self(), {keep_alive_tx_timeout}).

send_keep_alive(Socket) ->
    TimerRef = erlang:send_after(?KEEP_ALIVE_INTERNAL_TIMER, self(), {keep_alive_internal_timeout}),

    case gen_tcp:send(Socket, <<0:32>>) of
        ok ->
            {ok, TimerRef};
        {error, Reason} ->
            erlang:cancel(TimerRef),
            {error, Reason}
    end.

blocks_to_piece(Blocks) ->
    Sorted_blocks = lists:keysort(1, Blocks),

    ?INFO("TODO This might work ..."),
    Blocks_tmp = lists:foldl(fun({_Idx, Data}, Total) -> [Data| Total] end, [], Blocks),

    Blocks_list = lists:reverse(Blocks_tmp),

    ?INFO("Converting list to binary"),
    list_to_binary(Blocks_list).

complete_piece(State) ->
    Piece = blocks_to_piece(State#state.blocks),

    State#state.peer_srv_pid ! {peer_worker_piece_ready,
                                State#state.torrent_pid,
                                State#state.incoming_piece_hash,
                                Piece},

    State#state.piece_queue = [{New_piece_index,
                                New_piece_hash,
                                New_piece_total_size}| New_piece_queue],

    New_state = State#state{incoming_piece_blocks = [],
                            incoming_piece_downloaded_size = 0,
                            incoming_piece_hash = New_piece_hash,
                            incoming_piece_index = New_piece_index,
                            incoming_piece_queue = New_piece_queue,
                            incoming_piece_total_size = New_piece_total_size},

    {ok, New_state}.

%%% Callback module
init([ID, Info_hash, Peer_id, Socket, Torrent_pid]) when is_atom(ID) ->
    {ok, #state{id=ID,
                peer_id=Peer_id,
                peer_state=#peer_state{choked=true,
                                       interester=true},
                socket=Socket,
                torrent_info_hash=Info_hash,
                torrent_pid=Torrent_pid};

terminate(Reason, State) ->
    ?INFO("peer worker terminating: " ++ Reason),

    erlang:cancel_timer(State.keep_alive_external_ref),
    erlang:cancel_timer(State.keep_alive_internal_ref),

    ok = gen_tcp:close(State.socket),

    ?PEER_SRV ! {peer_w_terminate, State#state.id, State#state.current_piece},

    ok.

%% Synchronous
handle_call(Req, _From, State) ->
    {reply, ok};

%% Asynchronous
handle_cast({activate}) ->
    case gen_tcp:send(State#state.socket, <<19:32, "BitTorrent protocol", 0:64,
                                            State#state.torrent_info_hash:160,
                                            State#state.peer_id:160>>) of
        ok ->
            % This is automatically canceled if the process terminates
            Keep_alive_tx_ref = erlang:send_after(?KEEP_ALIVE_TX_TIMER,
                                                  self(),
                                                  {keep_alive_tx_timeout}),

            Keep_alive_rx_ref = erlang:send_after(?KEEP_ALIVE_RX_TIMER,
                                                  self(),
                                                  {keep_alive_rx_timeout}),

            {noreply, State#state{keep_alive_rx_ref=Keep_alive_rx_ref, keep_alive_tx_ref=Keep_alive_tx_ref}};
        {error, Reason} ->
            {stop, {error_handshake, Reason}}
    end;
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, _State) ->
    {ok}.

%% Timeout for when a keep alive message was expected from the peer
handle_info({keep_alive_rx_timeout}, State) ->
    {stop, peer_worker_timed_out, State};
%% Time to send another keep alive before the peer mark us as inactive
handle_info({keep_alive_tx_timeout}, State) ->
    case send_keep_alive(State.socket) of
        {ok, Timer_ref} ->
            New_state = State#state{keep_alive_tx_ref=Timer_ref};
            {noreply, New_state};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_info({peer_srv_piece, Index, Hash, Data}) ->
    case gen_tcp:send(State#state.socket) of
        ok ->
            0;
        {error, Reason} ->
            1
    end;

handle_info({peer_srv_piece, Data}, State) ->
    {noreply, State};

%% Messages from gen_tcp
% TODO:
% - Implement the missing fast extension
handle_info({tcp, S, <<>>}, State) ->
    New_keep_alive_rx_ref = reset_rx_keep_alive(State#state.keep_alive_rx_ref),

    New_state = State#state{keep_alive_rx_ref=New_keep_alive_rx_ref},

    {noreply, New_state};
handle_info({tcp, S, <<?HAVE, Piece_num:32/big-integer>>}, State) ->
    New_bitfield = ?BITFIELD:set_bit(Piece_num, 1, State#state.peer_bitfield),
    New_state = State#state{peer_bitfield = New_bitfield},

    State#state.peer_srv_pid ! {peer_worker_bitfield_update, New_bitfield},
    % TODO if we're sending a piece that is announced in a HAVE message, should
    % we cancel the tranmission or ignore it and wait for a CANCEL message?

    {noreply, New_state};
handle_info({tcp, S, <<?REQUEST, Index:32/big, Begin:32/big, Len:32/big>>}, State) ->
    case State#state.outgoing_piece_data == <<>> of
        true ->
            % If there's no cached piece, tell the peer_srv and wait for a
            % piece from the torrent_worker.
            State#state.peer_srv_pid ! {peer_worker_piece_request, self(), Index};
        false ->
            <<Begin, Bin:Len, Rest/binary>> = State#state.outgoing_piece_data,

            case gen_tcp:send(State#state.socket, Bin) of
                ok -> 1;
                {error, Reason} -> ?WARNING("failed to send a REQUEST response")
            end
    end,

    {noreply, State};
handle_info({tcp, S, <<?PIECE, Index:32/big-integer, Begin:32/big-integer, Data/binary>>}, State) ->
    ?INFO("piece index: " ++ Index ++ " begin: " ++ Begin),
    case Index == State#state.piece_index of
        true ->
            Size = State#state.piece_downloaded_size + binary:referenced_byte_size(Data),

            case Size of
                Size < State#state.piece_total_size ->
                    New_blocks = [{Begin, Data}| State#state.piece_blocks],
                    New_size = Size,

                    New_state = State#state{piece_blocks = New_blocks,
                                            piece_downloaded_size = New_size};
                Size == State#state.piece_total_size ->
                    {ok, New_state} = complete_piece(State);
                Size > State#state.piece_total_size ->
                    ?ERROR("Size of the piece is larger than it is supposed to. Discarding blocks"),
                    New_state = State
            end;
        false ->
            ?WARNING("Received blocks belonging to another piece")
    end,

    {noreply, New_state};
handle_info({tcp, S, <<?CHOKE>>}, State) ->
    State#state.peer_srv_pid ! {peer_worker_choke},
    {noreply, State};
handle_info({tcp, S, <<?UNCHOKE>>}, State) ->
    {unchoke};
    {noreply, State};
handle_info({tcp, S, <<?INTERESTED>>}, State) ->
    {interested};
    {noreply, State};
handle_info({tcp, S, <<?NOT_INTERESTED>>}, State) ->
    {not_interested};
    {noreply, State};
handle_info({tcp, S, <<?CANCEL, Index:32/big, Begin:32/big, Len:32/big>>}, State) ->
    {cancel, Index, Begin, Len};
    {noreply, State};
handle_info({tcp, S, <<19:32, "BitTorrent protocol", 0:64, Info_hash:160, Peer_id:160>>}, State) when
      State#state.received_handshake =:= false ->
    New_state = State#state{received_handshake=true},
    {noreply, New_state};
handle_info({tcp, S, <<?BITFIELD, Bitfield/binary>>}, State) ->
    ?INFO("peer[" ++ State#state.address ++ "] received bitfield " ++ )
    {noreply, State};
handle_info({tcp, S, <<?PORT, Port:16/big>>}, State) ->
    {noreply, State};
handle_info({tcp, S, Data}, State) ->
    io:format("unhandled peer[~s], message: ~p~n", [S, Data])
    {stop, peer_unhandled_message, State}.
handle_info({tcp_closed, S}, State) ->
    {stop, peer_connection_closed, State}.

%% Messages from the torrent worker
handle_info({torrent_worker_choke}) ->
    {ok, Msg} = msg_choke(),
handle_info({torrent_worker_unchoke}) ->

code_change(OldVsn, State, Extra) ->
    {ok}.

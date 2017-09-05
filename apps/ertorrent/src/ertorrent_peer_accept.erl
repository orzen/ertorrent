-module(ertorrent_peer_accept).

-behaviour(gen_server).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {accept_socket,
                listen_socket}).

-define(PEER_SSUP, ertorrent_peer_ssup).
-define(PEER_SRV, ertorrent_peer_srv).

init([Listen_socket, Port]) ->
    gen_server:cast(self(), {accept}),

    {ok, #state{listen_socket = Listen_socket}}.

terminate(_Reason, State) ->
    {ok, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({accept}, State = #state{listen_socket=Listen_socket}) ->
    {ok, Accept_socket} = gen_tcp:accept(Listen_socket),

    {noreply, State#state{accept_socket = Accept_socket}}.

handle_info({tcp, _S, <<19:32, "BitTorrent protocol", 0:64, Info_hash:160,
                        Peer_id:160>>}, State) ->
    % Spawn another listen socket
    ?PEER_SSUP:start_worker(State#state.listen_socket),

    % Spawn a new peer with the accept socket
    ?PEER_SRV:add_rx_peer(State#state.accept_socket,
                          Info_hash,
                          Peer_id),

    {stop, normal, State}.

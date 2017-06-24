-module(ertorrent_peer_tcp).

-behaviour(gen_server).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3).

-record(state, {accept_socket,
                listen_socket}).

-define(PEER_IN_SUP, ertorrent_peer_in_sup).
-define(PEER_SRV, ertorrent_peer_srv).

init(Listen_socket, Port) ->
    gen_server:cast(self(), {accept}).

    {ok, #state{listen_socket = Listen_socket}.

terminate() ->
    {ok, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({accept}, State = #state{listen_socket=Listen_socket}) ->
    {ok, Accept_socket} = gen_tcp:accept(Listen_socket),

    {noreply, State#state{accept_socket = Accept_socket}}.

handle_info({tcp,
             State#state.accept_socket,
             <<19:32, "BitTorrent protocol", 0:64, Info_hash:160, Peer_id:160>>},
            State) ->
    % Spawn another listen socket
    ?PEER_IN_SUP:start_worker(Listen_socket),

    % Spawn a new peer with the accept socket
    ?PEER_SRV:add_rx_peer(State#state.accept_socket,
                          Info_hash,
                          Peer_id),

    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

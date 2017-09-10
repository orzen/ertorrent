-module(ertorrent_peer_accept).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {accept_sockets::list(),
                accept_socket_timers::list(),
                listen_socket}).

-include("ertorrent_log.hrl").

-define(PEER_SSUP, ertorrent_peer_ssup).
-define(PEER_SRV, ertorrent_peer_srv).
-define(SETTINGS_SRV, ertorrent_settings_srv).
% Should be two minutes but we are generous
-define(PEER_WIRE_TIMEOUT, 130).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

close_sockets(State) ->
    % Start by closing the listen socket
    ok = gen_tcp:close(State#state.listen_socket),

    % If something is going wrong, this should be used to close non-transfered
    % socket.
    lists:foreach(
        fun(Accept_socket) ->
           ok = gen_tcp:close(Accept_socket)
        end,
    State#state.accept_sockets),

    % Cancel the timeout timers
    lists:foreach(
        fun(Time_ref) ->
            erlang:cancel_timer(Time_ref)
        end,
    State#state.accept_socket_timers),

    ok.

init(_Args) ->
    Port = ?SETTINGS_SRV:get_sync(peer_listen_port),

    case gen_tcp:listen(Port, []) of
        {ok, Listen_socket} ->
            gen_server:cast(self(), {accept}),

            {ok, #state{listen_socket = Listen_socket}};
        {error, Reason} ->
            {stop, Reason}
    end.

terminate(shutdown, State) ->
    ok = close_sockets(State),
    ok;
terminate(Reason, State) ->
    ok = close_sockets(State),
    ?ERROR("closing unexpectedly: " ++ Reason),
    ok.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast({accept}, State) ->
    case gen_tcp:accept(State#state.listen_socket) of
        {ok, Accept_socket} ->
            Timer_ref = erlang:send_after(?PEER_WIRE_TIMEOUT,
                                          self(),
                                          {peer_timed_out, Accept_socket}),

            New_accept_sockets = [Accept_socket| State#state.accept_sockets],
            New_accept_socket_timers = [Timer_ref| State#state.accept_socket_timers],

            {noreply, State#state{accept_sockets = New_accept_sockets,
                                  accept_socket_timers = New_accept_socket_timers}};
        {error, Reason} ->
            {stop, Reason, State}
    end.

handle_info({tcp, Socket, <<19:32, "BitTorrent protocol", 0:64, Info_hash:160,
                            Peer_id:160>>}, State) ->
    % Spawn another listen socket
    % ?PEER_SSUP:start_worker(State#state.listen_socket),
    gen_server:cast(self(), {accept}),

    % Spawn a new peer with the accept socket
    ?PEER_SRV:add_rx_peer(Socket,
                          Info_hash,
                          Peer_id),

    % Delete all occurrences of Socket (suger syntax for lists:subtract/2).
    New_accept_sockets = State#state.accept_sockets -- [Socket],

    {noreply, State#state{accept_sockets = New_accept_sockets}};

handle_info({peer_timed_out, Socket}, State) ->
    % Close the socket
    ok = gen_tcp:close(Socket),

    % Remove the socket from the list
    New_accept_sockets = lists:delete(Socket, State#state.accept_sockets),

    {noreply, State#state{accept_sockets = New_accept_sockets}}.

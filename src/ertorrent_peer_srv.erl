%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc peer_srv.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_peer_srv).

-behaviour(gen_server).

-export([
         add_rx_peers/3,
         peer_terminated/1,
         statem_terminated/1
        ]).

-export([start_link/0,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(SETTINGS_SRV, ertorrent_settings_srv).
-define(PEER_SUP, ertorrent_peer_sup).
-define(PEER_STATEM_SUP, ertorrent_peer_statem_sup).
-define(PEER_W, ertorrent_peer_worker).
-define(STATEM_SUP, ertorrent_statem_sup).
-define(TORRENT_SRV, ertorrent_torrent_srv).
-define(TORRENT_W, ertorrent_torrent_worker).
-define(UTILS, ertorrent_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
                block_length::integer(),
                processes=[], % Process pairs, [{peer, statem}]
                peer_id::string(),
                port_max::integer(),
                port_min::integer()
               }).

%%% Client API %%%
-spec add_rx_peers(Info_hash::string(), Peers::[tuple()],
                   Piece_length::integer()) -> ok.
add_rx_peers(Info_hash, Peers, Piece_length) when is_list(Peers) andalso
                                                  is_integer(Piece_length) ->
    gen_server:cast(?MODULE, {peer_s_add_rx_peers, self(), {Info_hash, Peers,
                                                            Piece_length}}).

peer_terminated(Peer_id) ->
    gen_server:cast(?MODULE, {peer_s_peer_terminated, Peer_id}).

statem_terminated(Statem_id) ->
    gen_server:cast(?MODULE, {peer_s_statem_terminated, Statem_id}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Internal functions

find_pair(Processes, Peer_id, undefined) ->
    case lists:keytake(Peer_id, 1, Processes) of
        {value, {Peer_id, Statem_id}, New_processes} -> {Statem_id, New_processes};
        false -> false
    end;
find_pair(Processes, undefined, Statem_id) ->
    case lists:keytake(Statem_id, 2, Processes) of
        {value, {Peer_id, Statem_id}, New_processes} -> {Peer_id, New_processes};
        false -> false
    end.

start_rx_peer(Torrent_pid, Block_length, Info_hash, Peer_id, Piece_length,
              {Address, Port}) when is_binary(Info_hash) ->
    Statem_ref = ?UTILS:gen_id(),
    Worker_ref = ?UTILS:gen_id(),

    case Statem_ref == undefined of
        true ->
            lager:error("~p: gen_id() generated broken ID '~p'", [?FUNCTION_NAME, Statem_ref, Worker_ref]);
        false ->
            ok
    end,

    Statem_ret = ?PEER_STATEM_SUP:start_child(Statem_ref, Worker_ref),

    Statem_res = case Statem_ret of
        {ok, Statem_pid} ->
            Statem_pid;
        {error, Reason_statem} ->
            lager:error("~p:~p: failed to spawn peer_statem: '~p'",
                        [?MODULE, ?FUNCTION_NAME, Reason_statem]),
            error
    end,

    case Statem_res /= error andalso
         ?PEER_SUP:start_child(Worker_ref, Block_length, rx, Info_hash,
                               Peer_id, Piece_length, {Address, Port},
                               Torrent_pid, Statem_res) of
        % TODO Atm the handling of the {ok, _} responses is redundant.
        % It's unlikely that both will be used however in the writing
        % moment this cannot be determined and therefore both are taken
        % into consideration until one could be ruled out.
        {ok, Peer_pid} ->
            lager:debug("~p: ~p: connecting to peer '~p:~p'", [?MODULE,
                                                               ?FUNCTION_NAME,
                                                               Address,
                                                               Port]),
            ok = ?PEER_W:connect(Peer_pid),

            {
             {info_hash, Info_hash},
             {peer_ipv4, {Address, Port}},
             {statem_id, Statem_res},
             {peer_w_id, Worker_ref}
            };
        % This is rather unexpected so log it until it is clear when it happens
        {error, Reason_sup} ->
            lager:error("peer_srv failed to spawn a peer_worker (rx), check the peer_sup. reason: '~p'",
                        [Reason_sup]),

            error;
        Catch ->
            lager:error("UNHANDLED CLAUSE '~p'", [Catch]),
            error
    end.

%%% Callback module
init(_Args) ->
    {block_length, Block_length} = ?SETTINGS_SRV:get_sync(block_length),
    {peer_id, Peer_id} = ?SETTINGS_SRV:get_sync(peer_id),

    {ok, #state{block_length = Block_length, peer_id = Peer_id}, hibernate}.

handle_call(Req, From, State) ->
    lager:warning("~p:~p: unhandled call request '~p', from '~p'",
                  [?MODULE, ?FUNCTION_NAME, Req, From]),
    {noreply, State}.

-spec handle_cast({peer_s_add_rx_peers, From_id::atom(), {Info_hash::string(), Peers::[tuple()],
                   Piece_length::integer()}}, State::term()) -> ok.
handle_cast({peer_s_add_rx_peers, From, {Info_hash, Peers, Piece_length}}, State) ->
    lager:debug("~p: ~p: peer_s_add_rx_peers", [?MODULE, ?FUNCTION_NAME]),

    % Setup peer connections, succeeding peers will return a reference and
    % failing 'error'.
    Fold_results = fun(Peer_socket, Acc) ->
                       Ref = start_rx_peer(From, State#state.block_length,
                                           Info_hash, State#state.peer_id,
                                           Piece_length, Peer_socket),

                       [Ref| Acc]
                   end,
    Peer_results = lists:foldl(Fold_results, [], Peers),

    % Create a list of connected peer workers
    Filter_errors = fun(Res) ->
                        case Res of
                            error -> false;
                            _ -> true
                        end
                    end,
    Succeeded_peers = lists:filter(Filter_errors, Peer_results),

    % Count the amount of failed peer workers
    Nbr_of_failed_peers = length(Peers) - length(Succeeded_peers),

    case Nbr_of_failed_peers > 0 of
        true ->
            % Request new peers corresponding to the amount of the failed peers
            ok = ?TORRENT_W:request_peers(From, Nbr_of_failed_peers);
        false ->
            lager:debug("~p: ~p: reached expected amount of running peers",
                        [?MODULE, ?FUNCTION_NAME])
    end,

    % Send back the list of connected peers
    From ! {peer_s_rx_peers, Succeeded_peers},

    New_processes = lists:concat([Succeeded_peers, State#state.processes]),
    New_state = State#state{processes = New_processes},

    {noreply, New_state, hibernate};

% @doc Adding a transmitting peer worker. The socket is already accepted by the
% peer accept-process. Note: When mentioning the terms receiving and
% transmitting, about peers, it refers to the function of the peer worker
% process and not the actual peer on the other side of the wire.
% @end
%handle_cast({add_tx_peer, From, {Socket, Info_hash}}, State) ->
%    ID = erlang:unique_integer(),
%
%    % Retreive the address and port from the socket
%    case inet:peername(Socket) of
%        {ok, {S_address, S_port}} ->
%            Address = S_address,
%            Port = S_port;
%        {error, Reason_peername} ->
%            lager:warning("~p:~p: failed to retreive address and port form tx peer's socket: '~p'",
%                          [?MODULE, ?FUNCTION_NAME, Reason_peername]),
%
%            % Set fail-over values for Address and Port, these values are
%            % mainly for information so should not be vital.
%            Address = unknown,
%            Port = unknown
%    end,
%
%    Peers = State#state.peers,
%
%    % Validate the Info_hash
%    case ?TORRENT_SRV:member_by_info_hash(Info_hash) of
%        true ->
%            case ?PEER_SUP:start_child(ID, tx, Info_hash,
%                                       State#state.peer_id,
%                                       {socket, Socket}, From) of
%                {ok, Peer_pid} ->
%                    New_state = State#state{peers=[{ID,
%                                                    Peer_pid,
%                                                    Address,
%                                                    Port,
%                                                    Info_hash}| Peers]};
%                {ok, Peer_pid, _Info} ->
%                    New_state = State#state{peers=[{ID,
%                                                    Peer_pid,
%                                                    Address,
%                                                    Port,
%                                                    Info_hash}| Peers]};
%                {error, Reason_sup} ->
%                    lager:error("~p:~p: failed to spawn a peer_worker (tx), check the peer_sup. reason: '~p'",
%                                 [?MODULE, ?FUNCTION_NAME, Reason_sup]),
%
%                    New_state = State
%            end;
%        false ->
%            ?WARNING("incoming peer request for a non-existing torrent with hash:"
%                     ++ Info_hash),
%
%            New_state = State
%    end,
%    {noreply, New_state, hibernate};

handle_cast({peer_s_peer_terminated, Peer_id}, State) ->
    case find_pair(State#state.processes, Peer_id,
                                          undefined) of
        {Statem_id, New_processes} ->
            ok = ?STATEM_SUP:terminate_child(Statem_id),
            New_state = State#state{processes = New_processes};
        false ->
            % Ignore the last process of the pair when it reports that it is
            % terminating.
            New_state = State
    end,

    {noreply, New_state, hibernate};

handle_cast({peer_s_statem_terminated, Statem_id}, State) ->
    case find_pair(State#state.processes, undefined,
                                          Statem_id) of
        {Peer_id , New_processes} ->
            ok = ?PEER_SUP:terminate_child(Peer_id),
            New_state = State#state{processes = New_processes};
        false ->
            % Ignore the last process of the pair when it reports that it is
            % terminating.
            New_state = State
    end,

    {noreply, New_state, hibernate};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({'EXIT', _ParentPid, shutdown}, State) ->
    {stop, shutdown, State};
handle_info(_Info, _State) ->
    ok.

terminate(Reason, _State) ->
    lager:debug("~p:~p: terminating '~p'", [?MODULE, ?FUNCTION_NAME, Reason]),
    normal.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

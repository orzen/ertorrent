-module(ertorrent_tracker_udp_worker).
-behaviour(gen_server).

% TODO
% - Implement scrape
% - Make sure not to stack announce requests.
%   Check if a transaction exists before creating another one with the client timeout

% User API
-export([
         connect/2,
         disconnect/2,
         %get_peers/2,
         scrape/1,
         start_link/1,
         stop/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-define(ANNOUNCE_INTERVAL, 120000).
-define(TIMEOUT, 120000).

-include_lib("eunit/include/eunit.hrl").
-include("ertorrent_modules.hrl").
-include("ertorrent_tracker.hrl").
-include("ertorrent_internal.hrl").

-record(state, {
          id :: atom(),
          clients :: map(),
          dest,
          inet_version:: inet | inet6,
          interval = ?ANNOUNCE_INTERVAL,
          tracker_srv,
          transactions = #{} :: map(),
          postponed = [] :: list(),
          src_port :: integer()
         }).

-record(transaction, {
          client = undefined,
          id = undefined,
          request = undefined,
          attempts = 0,
          timer = undefined,
          type = undefined
         }).

-record(dest, {
          host,
          conn_id,
          socket,
          port :: integer()
         }).

% User API

connect(Worker_ref, Client) ->
    gen_server:cast(Worker_ref, {connect, self(), Client}).

disconnect(Worker_ref, Client) ->
    gen_server:cast(Worker_ref, {disconnect, self(), Client}).

scrape(_Args) ->
    ok.

start_link(Args = #'tracker.udp.args'{id = ID}) ->
    gen_server:start_link({local, ID}, ?MODULE, Args, []).

stop(Worker_ref) ->
    gen_server:stop(Worker_ref).

% Internal Functions

handle_postponed(Postponed) ->
    F = fun({Func, Args}, Acc) ->
                {Key, Value} = Func(Args),
                Acc#{Key => Value}
        end,
    lists:foldl(F, #{}, Postponed).

request_data1(Client, Interval) ->
    Client ! #'tracker.worker.announce'{ tracker_worker_pid = self() },
    Timer = erlang:send_after(Interval,
                              self(),
                              #'tracker.worker.announce_expired'{ client_pid = Client }),
    {Client, Timer}.

request_data({Client, Interval, undefined}) ->
    request_data1(Client, Interval);
request_data({Client, Interval, Timer_ref}) ->
    erlang:cancel_timer(Timer_ref),
    request_data1(Client, Interval).

request_or_postpone(Client, State = #state{clients = Clients,
                                           dest = Dest,
                                           interval = Interval}) ->
    case Dest#dest.conn_id /= undefined of
        true ->
            {Client, Timer} = request_data({Client, Interval, undefined}),
            Clients2 = Clients#{Client => Timer},
            {noreply, State#state{clients = Clients2}, hibernate};
        false ->
            New_postponed = [{fun request_data/1,
                              {Client, Interval, undefined}}| State#state.postponed],
            {noreply, State#state{postponed = New_postponed}, hibernate}
    end.

send_transaction(#dest{host = Host, port = Port, socket = Socket},
                 Trans = #transaction{attempts = Attempts, id = ID, request = Req}) ->
    case gen_udp:send(Socket, Host, Port, Req) of
        ok ->
            Timer = ?TRACKER_UDP:timeout(ID, Attempts),
            {ok, Trans#transaction{timer = Timer, attempts = Attempts + 1}};
        {error, closed} ->
            lager:info("~s:~s(~p): tracker closed the connection",
                        [?MODULE, ?FUNCTION_NAME, self()]),
            {error, closed};
        {error, Reason} ->
            lager:error("~s:~s(~p): tracker worker failed to send transaction: '~w'",
                        [?MODULE, ?FUNCTION_NAME, self(), Reason]),
            {error, Reason};
        Unhandled ->
            lager:error("~s(~p): '~s' unhandled error '~p'",
                        [?MODULE, self(), ?FUNCTION_NAME, Unhandled]),
            {error, unhandled}
    end.

retry_transaction(Dest, Transaction = #transaction{attempts = Attempts}) ->
    case Attempts =< 8 of
        true ->
            send_transaction(Dest, Transaction);
        false ->
            {error, max_retries}
    end.

new_transaction() ->
    {ok, #transaction{id = ?TRACKER_UDP:gen_transaction_id()}}.

send_announce(Dest, #'tracker.worker.announce_data'{client_pid = Client,
                                                    info_hash = Info_hash,
                                                    peer_id = Peer_id,
                                                    downloaded = Downloaded,
                                                    left = Left,
                                                    uploaded = Uploaded,
                                                    event = Event}) ->
    {ok, T} = new_transaction(),

    IPv4 = 0,
    Key = 0,
    Num_want = -1,
    {ok, Port} = ?SETTINGS_SRV:get_conf(peer_listen_port),

    E = case Event of
        started -> 2;
        completed -> 1;
        stopped -> 3;
        none -> 0
    end,

    Req = #'tracker.udp.msg.announce_req'{connection_id = Dest#dest.conn_id,
                                          transaction_id = T#transaction.id,
                                          info_hash = Info_hash,
                                          peer_id = Peer_id,
                                          downloaded = Downloaded,
                                          left = Left,
                                          uploaded = Uploaded,
                                          event = E,
                                          ipv4 = IPv4,
                                          key = Key,
                                          num_want = Num_want,
                                          port = Port},
    {ok, Announce_req} = ?TRACKER_UDP:announce_req(Req),

    T2 = T#transaction{client = Client,
                       request = Announce_req},

    send_transaction(Dest, T2).

send_connect(Dest) ->
    {ok, Transaction_id, Connect_req} = ?TRACKER_UDP:connect_req(),
    lager:info("~s(~w): sending connect '~w'", [?MODULE, self(), Transaction_id]),
    Transaction = #transaction{id = Transaction_id,
                               attempts = 0,
                               timer = undefined,
                               request = Connect_req},
    send_transaction(Dest, Transaction).

% Behaviour callbacks

init(#'tracker.udp.args'{id = ID,
                         host = Host,
                         dst_port = Dst_port,
                         inet_version = Version,
                         socket = Socket,
                         src_port = Src_port,
                         tracker_srv = Server}) ->
    lager:debug("~s:~s(~p): init", [?MODULE, ?FUNCTION_NAME, self()]),
    Dest = #dest{socket = Socket,
                 conn_id = undefined,
                 host = Host,
                 port = Dst_port},

    case send_connect(Dest) of
        {ok, Transaction} ->
            State = #state{id = ID,
                           inet_version = Version,
                           dest = Dest,
                           transactions = #{Transaction#transaction.id => Transaction},
                           tracker_srv = Server,
                           src_port = Src_port},
            {ok, State, hibernate};
        {error, Reason} ->
            {stop, Reason}
    end.

% TODO add terminate that will close the connection

handle_call(Request, From, State) ->
    lager:error("~s(~p): unhandled call from '~p' '~p'",
                [?MODULE, self(), From, Request]),
    {stop, unhandled_call, State}.

handle_cast(Request, State) ->
    lager:error("~s(~p): unhandled cast '~p'",
                [?MODULE, self(), Request]),
    {stop, unhandled_cast, State}.

% Send reply if Client is set, undefined is used for connects
transaction_reply(undefined, _Msg) ->
    ok;
transaction_reply(Client, Msg) ->
    Client(Msg).

handle_transaction(Transaction_id, Transactions, Message) ->
    case maps:take(Transaction_id, Transactions) of
        {#transaction{client=Client,
                      timer=Timer}, Transactions2} ->
            erlang:cancel_timer(Timer),

            % Reply if a client is specified
            transaction_reply(Client, Message),

            Transactions2;
        error ->
            Transactions
    end.

% TODO validate IP from the sender
handle_info({udp, _Socket, _IP, _InPortNo, Packet},
            State = #state{dest = Dest,
                           inet_version = Inet_version,
                           postponed = Postponed,
                           transactions = Transactions}) ->
    case ?TRACKER_UDP:parse_res(Packet, Inet_version) of
        {ok, Msg = #'tracker.udp.msg.announce_res'{interval = Interval,
                                                   transaction_id = Transaction_id}} ->

            Transactions2 = handle_transaction(Transaction_id,
                                               Transactions,
                                               Msg),

            % TODO Update the interval
            % TODO CONTINUE HERE set new announce with Interval

            {noreply, State#state{interval = Interval,
                                  transactions = Transactions2}, hibernate};
        {ok, Msg = #'tracker.udp.msg.scrape_res'{transaction_id = Transaction_id}} ->
            Transactions2 = handle_transaction(Transaction_id,
                                               Transactions,
                                               Msg),
            {noreply, State#state{transactions = Transactions2}, hibernate};
        {ok, Msg = #'tracker.udp.msg.error'{transaction_id = Transaction_id,
                                            message = Message}} ->
            lager:warning("tracker worker received an error response '~w'", [Message]),

            % TODO figure out if errors should be handled here or in the torrent_worker.
            %{#transaction{client = Client, type = Type},
            % New_transactions} = maps:take(Transaction_id, Transactions),

            Transactions2 = handle_transaction(Transaction_id,
                                               Transactions,
                                               Msg),

            {noreply, State#state{transactions=Transactions2}, hibernate};
        {ok, Msg = #'tracker.udp.msg.connect_res'{connection_id = Connection_id,
                                                  transaction_id = Transaction_id}} ->
            lager:info("~s(~w): connect response '~w'", [?MODULE, self(), Transaction_id]),
            Transactions2 = handle_transaction(Transaction_id,
                                               Transactions,
                                               Msg),

            Clients = handle_postponed(Postponed),

            {noreply, State#state{clients = Clients,
                                  dest = Dest#dest{conn_id = Connection_id},
                                  transactions = Transactions2}, hibernate};
        {error, Bad_res} ->
            lager:error("~s(~w): failed to parse response '~p'", [?MODULE, self(), Bad_res]),
            {noreply, State, hibernate}
    end;
handle_info(#'tracker.udp.transaction_expired'{transaction_id = Transaction_id},
            State = #state{transactions = Transactions}) ->
    lager:info("~s(~w): transaction expired '~w'", [?MODULE, self(), Transaction_id]),
    #{Transaction_id := Transaction} = Transactions,
    case retry_transaction(State#state.dest, Transaction) of
        {ok, Transaction2} ->
            Transactions2 = Transactions#{Transaction_id => Transaction2},
            {noreply, State#state{transactions = Transactions2}, hibernate};
        {error, closed} ->
            lager:info("~s(~p): tracker closed the connection",
                       [?MODULE, self()]),
            {stop, {shutdown, tracker_closed}, State};
        {error, max_retries} ->
            lager:info("~s(~p): tracker unresponsive",
                       [?MODULE, self()]),
            {stop, {shutdown, tracker_timeout}, State};
        {error, Reason} ->
            lager:info("~s(~p): transaction retry failed '~p'",
                       [?MODULE, self(), Reason]),
            {stop, tracker_timeout, State}
    end;
handle_info(Msg = #'tracker.worker.announce_data'{},
            State = #state{transactions = Transactions}) ->
    {ok, Transaction} = send_announce(State#state.dest, Msg),

    Transactions2 = Transactions#{Transaction#transaction.id => Transaction},

    {noreply, State#state{transactions = Transactions2}, hibernate};
handle_info(#'tracker.worker.announce_expired'{ client_pid = Client}, State) ->
    request_or_postpone(Client, State);
handle_info(#'tracker.worker.subscribe'{client_pid = Client,
                                        reply = #reply{func = Func}},
            State = #state{id = Own_id}) ->
    Ret = request_or_postpone(Client, State),
    Func(#'tracker.worker.subscribed'{client_pid = Client,
                                      tracker_worker_pid = Own_id}),
    Ret;
handle_info(#'tracker.worker.unsubscribe'{client_pid = Client, reply = Reply},
            State = #state{clients = Clients, transactions = Trans}) ->
    % Cancel the announce timer
    {Timer, Clients2} = maps:take(Client, Clients),
    erlang:cancel_timer(Timer),

    % Clean up ongoing transactions
    Filter = fun(_K,#transaction{client = C}) -> Client /= C end,
    Trans2 = maps:filter(Filter, Trans),

    Reply ! #'tracker.worker.unsubscribed'{ client_pid = Client},

    {noreply, State#state{clients = Clients2, transactions = Trans2}, hibernate};
handle_info({udp, _Socket, _IP, _InPortNo, _AncData, _Packet}, State) ->
    lager:error("~s(~p): unhandled UDP configuration AncData", [?MODULE, self()]),
    {stop, unhandled_udp, State};
handle_info(Msg, State) ->
    lager:error("~s(~p): unhandled info '~p'", [?MODULE, self(), Msg]),
    {stop, unhandled_info, State}.


% Tests
%-ifdef(EUNIT).
%
%init_test() ->
%    {Res, Ref} = ?MODULE:start(<<"foobar">>, 1234, 5678, inet),
%    ResX = ?MODULE:stop(Ref),
%
%    % TODO investigate how to separate tests from code
%    % TODO replace io:format with debugFmt
%    % TODO investigate how unit test behaviours properly (e.g. gen_statem)
%    io:format("RES '~w'~n", [Res]),
%    io:format("RESX '~w'~n", [ResX]),
%
%    ?assert(Res == ok).
%
%-endif.

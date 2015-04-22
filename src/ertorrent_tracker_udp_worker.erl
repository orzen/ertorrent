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

-record(state, {
          clients :: map(),
          dest,
          inet_version:: inet | inet6,
          tracker_srv,
          transactions = #{} :: map(),
          postponed = [] :: list(),
          src_port :: integer()
         }).

-record(transaction, {
          client,
          id,
          request,
          attempts,
          timer,
          type
         }).

-record(dest, {
          host,
          conn_id,
          socket,
          port :: integer()
         }).

-include_lib("eunit/include/eunit.hrl").
-include("ertorrent_modules.hrl").
-include("ertorrent_tracker.hrl").

-define(ANNOUNCE_INTERVAL, 120000).
-define(TIMEOUT, 120000).

% User API

connect(Worker_ref, Client) ->
    gen_server:cast(Worker_ref, {connect, self(), Client}).

disconnect(Worker_ref, Client) ->
    gen_server:cast(Worker_ref, {disconnect, self(), Client}).

scrape(_Args) ->
    ok.

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop(Worker_ref) ->
    gen_server:stop(Worker_ref).

% Internal Functions

handle_postponed(Postponed) ->
    F = fun({Func, Args}, Acc) ->
                {Key, Value} = Func(Args),
                Acc#{Key => Value}
        end,
    lists:foldl(F, #{}, Postponed).

request_data1(Client) ->
    Client ! #'tracker.worker.data_request'{ tracker_pid = self() },
    Timer = erlang:send_after(?ANNOUNCE_INTERVAL,
                              self(),
                              #'tracker.worker.announce_expired'{ client_pid = Client }),
    {Client, Timer}.

request_data({Client, undefined}) ->
    request_data1(Client);
request_data({Client, Timer_ref}) ->
    erlang:cancel_timer(Timer_ref),
    request_data1(Client).

request_or_postpone(Client, State = #state{clients = Clients, dest = Dest}) ->
    case Dest#dest.conn_id /= undefined of
        true ->
            {Client, Timer} = request_data({Client, undefined}),
            Clients2 = Clients#{Client => Timer},
            {noreply, State#state{clients = Clients2}, hibernate};
        false ->
            New_postponed = [{fun request_data/1,
                              {Client, undefined}}| State#state.postponed],
            {noreply, State#state{postponed = New_postponed}, hibernate}
    end.

send_transaction(#dest{host = Host, port = Port, socket = Socket},
                 Trans = #transaction{attempts = Attempts, id = ID, request = Req}) ->
    case gen_udp:send(Socket, Host, Port, Req) of
        ok ->
            Timer = ?TRACKER_UDP:timeout(ID, Attempts),
            {ok, Trans#transaction{timer = Timer, attempts = Attempts + 1}};
        {error, Reason} ->
            lager:error("tracker worker failed to send transaction: '~w'", [Reason]),
            {error, Reason}
    end.

retry_transaction(Dest, Transaction = #transaction{attempts = Attempts}) ->
    case Attempts =< 8 of
        true ->
            send_transaction(Dest, Transaction);
        false ->
            {error, max_retries}
    end.

send_announce(Dest, Data) ->
    {ok, Transaction_id,
     Announce_req} = ?TRACKER_UDP:announce_req(Dest#dest.conn_id, Data),
    Transaction = #transaction{id = Transaction_id,
                               attempts = 0,
                               timer = undefined,
                               request = Announce_req},
    send_transaction(Dest, Transaction).

send_connect(Dest) ->
    {ok, Transaction_id, Connect_req} = ?TRACKER_UDP:connect_req(),
    lager:info("~s(~w): sending connect '~w'", [?MODULE, self(), Transaction_id]),
    Transaction = #transaction{id = Transaction_id,
                               attempts = 0,
                               timer = undefined,
                               request = Connect_req},
    send_transaction(Dest, Transaction).

% Behaviour callbacks

init(#'tracker.udp.args'{host = Host,
                         dst_port = Dst_port,
                         inet_version = Version,
                         socket = Socket,
                         src_port = Src_port,
                         tracker_srv = Server}) ->
    Dest = #dest{socket = Socket,
                 conn_id = undefined,
                 host = Host,
                 port = Dst_port},

    case send_connect(Dest) of
        {ok, Transaction} ->
            Transaction2 = Transaction#transaction{client = Server},

            State = #state{inet_version = Version,
                           dest = Dest,
                           transactions = #{Transaction2#transaction.id => Transaction2},
                           tracker_srv = Server,
                           src_port = Src_port},
            {ok, State, hibernate};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(Request, From, State) ->
    lager:error("Unhandled call from '~w' '~w'", [From, Request]),
    {stop, unhandled_call, State}.

handle_cast({client_data, From, Data}, State = #state{transactions = Transactions}) ->
    {ok, Transaction} = send_announce(State#state.dest, Data),
    Transaction2 = Transaction#transaction{client = From},

    Transactions2 = Transactions#{Transaction2#transaction.id => Transaction2},

    {noreply, State#state{transactions = Transactions2}, hibernate};
handle_cast(#'tracker.udp.connect'{client_pid = Client, reply = Reply},
            State) ->
    Ret = request_or_postpone(Client, State),
    Reply ! #'tracker.udp.connected'{ client_pid = Client },
    Ret;
handle_cast(#'tracker.udp.disconnect'{client_pid = Client, reply = Reply},
            State = #state{clients = Clients, transactions = Trans}) ->
    % Cancel the announce timer
    {Timer, Clients2} = maps:take(Client, Clients),
    erlang:cancel_timer(Timer),

    % Clean up ongoing transactions
    Filter = fun(_K,#transaction{client = C}) -> Client /= C end,
    Trans2 = maps:filter(Filter, Trans),

    Reply ! #'tracker.udp.disconnected'{ client_pid = Client},

    {noreply, State#state{clients = Clients2, transactions = Trans2}, hibernate};
handle_cast(Request, State) ->
    lager:error("Unhandled cast '~w'", [Request]),
    {stop, unhandled_cast, State}.

% Send reply if Client is set
transaction_reply(undefined, Msg) ->
    ok;
transaction_reply(Client, Msg) ->
    Client ! Msg.

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
                           postponed = Postponed,
                           transactions = Transactions}) ->
    case ?TRACKER_UDP:parse_res(Packet, State#state.inet_version) of
        {ok, #'tracker.udp.msg.announce_res'{transaction_id = Transaction_id,
                                             interval = Interval,
                                             leechers = Leechers,
                                             seeders = Seeders,
                                             peers = Peers,
                                             peers6 = Peers6}} ->

            % Send the peers back to the torrent_worker aka the client
            % TODO rename to #'tracker.worker.announce_res'{} ?
            Message = #'tracker.worker.peers'{leechers = Leechers,
                                              seeders = Seeders,
                                              peers = Peers,
                                              peers6 = Peers6},
            Transactions2 = handle_transaction(Transaction_id,
                                               Transactions,
                                               Message),

            % TODO Update the interval
            % TODO CONTINUE HERE set new announce with Interval

            {noreply, State#state{interval = Interval2,
                                  transactions = Transactions}, hibernate};
        {ok, {scrape_res, Data}} ->
            handle_res(Data, State);
        {ok, #'tracker.udp.msg.scrape_res'{transaction_id = Transaction_id,
                                           data = Data}} ->
            % TODO Is handle_res still relevant?
            %handle_res(Data, State);
            Client ! #'tracker.worker.scrape_res'{data = Data}
        {ok, {error_res, [{transaction_id, Transaction_id},
                          {message, Message}]}} ->
            lager:warning("tracker worker received an error response '~w'", [Message]),
            {#transaction{client = Client, type = Type},
             New_transactions} = maps:take(Transaction_id, Transactions),

            Client ! {tracker_udp_w, {error, Type, Message}},

            {noreply, State#state{transactions=New_transactions}, hibernate};
        {ok, {connect_res, [{transaction_id, Transaction_id},
                            {connection_id, Connection_id}]}} ->
            lager:info("~s(~w): connect response '~w'", [?MODULE, self(), Transaction_id]),
            Clients = handle_postponed(Postponed),
            {#transaction{timer = Timer},
             Transactions2} = maps:take(Transaction_id,
                                        Transactions),

            erlang:cancel_timer(Timer),

            {noreply, State#state{clients = Clients,
                                  dest = Dest#dest{conn_id = Connection_id},
                                  transactions = Transactions2}, hibernate};
        {error, Bad_res} ->
            lager:error("~s(~w): failed to parse response '~p'", [?MODULE, self(), Bad_res]),
            {noreply, State, hibernate}
    end,

    Func
handle_info(#'tracker.worker.announce_expired'{ client_pid = Client}, State) ->
    request_or_postpone(Client, State);
handle_info(#'tracker.udp.transaction_expired'{transaction_id = Transaction_id},
            State = #state{transactions = Transactions}) ->
    lager:info("~s(~w): transaction expired '~w'", [?MODULE, self(), Transaction_id]),
    #{Transaction_id := Transaction} = Transactions,
    case retry_transaction(State#state.dest, Transaction) of
        {ok, Transaction2} ->
            Transactions2 = Transactions#{Transaction_id => Transaction2},
            {noreply, State#state{transactions = Transactions2}, hibernate};
        {error, max_retries} ->
            lager:error("tracker unresponsive"),
            {stop, tracker_timeout, State};
        {error, Reason} ->
            lager:error("transaction retry failed '~w'", [Reason]),
            {stop, tracker_timeout, State}
    end;
handle_info({udp, _Socket, _IP, _InPortNo, _AncData, _Packet}, State) ->
    lager:error("Unhandled UDP configuration AncData"),
    {stop, unhandled_udp, State};
handle_info(Info, State) ->
    lager:error("Unhandled info '~w'", Info),
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

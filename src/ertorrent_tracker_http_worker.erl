-module(ertorrent_tracker_http_worker).
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
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-record(state, {
          conn_pid,
          clients :: map(),
          host,
          port,
          tracker_srv,
          transactions :: map()
         }).

-record(transaction, {
          client_pid,
          id, % gun StreamRef
          query, % HTTP query
          timer,
          type :: 'announce' | 'scrape'
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

start_link(Args = #'tracker.http.args'{}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop(Worker_ref) ->
    gen_server:stop(Worker_ref).

% Internal Functions

% handle_res(Data, State = #state{transactions = Trans}) ->
%     [{transaction_id, Trans_id}| Rest] = Data,
%     {#transaction{client_pid = Client,
%                   type = Type,
%                   timer = Timer}, Trans2} = maps:take(Trans_id, Trans),
%     erlang:cancel_timer(Timer),
%     Client ! {tracker_udp_w, {Type, Rest}},
%     {noreply, State#state{transactions=Trans2}, hibernate}.

request_data1(Client) ->
    Client ! #'tracker.worker.data_request'{ tracker_pid = self() },
    Timer = erlang:send_after(?ANNOUNCE_INTERVAL,
                              self(),
                              #'tracker.worker.announce_expired'{ client_pid = Client }),
    {Client, Timer}.
request_data(Client, undefined) ->
    request_data1(Client);
request_data(Client, Timer_ref) ->
    erlang:cancel_timer(Timer_ref),
    request_data1(Client).

announce(Conn_pid, Data = #'tracker.http.announce'{}) ->
    {ok, Query} = ?TRACKER_HTTP:announce_query(Data),
    Stream_ref = gun:get(Conn_pid, Query),
    #transaction{ id = Stream_ref, query = Query, type = announce }.

% Behaviour callbacks

init([#'tracker.http.args'{
                           host = Host,
                           port = Port,
                           tracker_srv = Tracker_srv,
                           tls = TLS
                          }]) ->
    {ok, Conn_pid} = case TLS of
        true ->
            gun:open(Host, Port, #{transport => tls});
        false ->
            gun:open(Host, Port)
    end,

    State = #state{
               conn_pid = Conn_pid,
               host = Host,
               port = Port,
               tracker_srv = Tracker_srv
              },
    {ok, State, hibernate}.

terminate(Reason, State = #state{conn_pid = Conn_pid}) ->
    lager:info("~s(~w): terminating, reason '~s'", [?MODULE, self(), Reason]),
    gun:close(Conn_pid),
    {ok, State}.

handle_call(Request, From, State) ->
    lager:error("Unhandled call from '~w' '~w'", [From, Request]),
    {stop, "Unhandled call", State}.

handle_cast(#'tracker.worker.client_data'{client_pid = Client, data = Data},
            State = #state{conn_pid = Conn_pid, transactions = Transactions}) ->
    Transaction = announce(Conn_pid, Data),
    Transaction2 = Transaction#transaction{client_pid = Client},

    Transactions2 = Transactions#{Transaction2#transaction.id => Transaction2},

    {noreply, State#state{transactions = Transactions2}, hibernate};
handle_cast(#'tracker.http.connect'{reply = Reply, client_pid = Client},
            State = #state{clients = Clients}) ->
    {Client, Timer} = request_data(Client, undefined),
    Clients2 = Clients#{Client => Timer},

    Reply ! #'tracker.http.connected'{client_pid = Client},

    {noreply, State#state{clients = Clients2}, hibernate};
handle_cast(#'tracker.http.disconnect'{client_pid = Client, reply = Reply},
            State = #state{clients = Clients, transactions = Trans}) ->
    % Cancel the announce timer
    {Timer, Clients2} = maps:take(Client, Clients),
    erlang:cancel_timer(Timer),

    % Clean up ongoing transactions
    Filter = fun(_K,#transaction{client_pid = C}) -> Client /= C end,
    Trans2 = maps:filter(Filter, Trans),

    Reply ! #'tracker.http.disconnected'{ client_pid = Client },

    {noreply, State#state{clients = Clients2, transactions = Trans2}, hibernate};
handle_cast(Request, State) ->
    lager:error("Unhandled cast '~w'", [Request]),
    {stop, unhandled_cast, State}.

handle_info({gun_response, _Conn_pid, _Stream_ref, fin, Status, _Headers}, State) ->
    lager:info("~s(~w): gun_response 'fin' status '~w'", [?MODULE, self(), Status]),
    {noreply, State};
handle_info({gun_response, _Conn_pid, _Stream_ref, nofin, Status, _Headers}, State) ->
    lager:info("~s(~w): gun_response 'nofin' status '~w'", [?MODULE, self(), Status]),
    {noreply, State};
handle_info({gun_data, _Conn_pid, _Stream_ref, nofin, Data}, State) ->
    lager:info("~s(~w): gun_data 'nofin' data '~s'", [?MODULE, self(), Data]),
    {noreply, State};
handle_info({gun_data, _Conn_pid, _Stream_ref, fin, Data}, State) ->
    lager:info("~s(~w): gun_data 'fin' data '~s'", [?MODULE, self(), Data]),
    {noreply, State};
handle_info({'DOWN', M_ref, process, _Conn_pid, Reason}, State) ->
    lager:info("~s(~w): gun, connection lost, mref '~w' reason '~s'",
               [?MODULE, self(), M_ref, Reason]),
    {stop, connection_terminated, State};
handle_info(Info, State) ->
    lager:error("Unhandled info '~w'", Info),
    {stop, unhandled_info, State}.


% Tests
%-ifdef(EUNIT).
%
%-endif.

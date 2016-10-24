-module(ertorrent_tracker_udp_worker).
-behaviour(gen_server).

% User API
-export([
         announce/1,
         scrape/1,
         start/4,
         stop/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
          transactions::map(),
          last_transaction_id,
          socket,
          inet_version:: inet | inet6,
          address,
          src_port,
          dst_port
         }).

-include_lib("eunit/include/eunit.hrl").

% User API

announce(_Args) ->
    ok.

scrape(_Args) ->
    ok.

start_link(Server, Socket, Address, Src_port, Dst_port, Version) when
      is_binary(Address) andalso
      is_integer(Src_port) andalso
      is_integer(Dst_port) andalso
      (Version == inet orelse Version == inet6) ->
    Args = [
            {tracker_srv, Server}
            {socket, Socket},
            {address, Address},
            {src_port, Src_port},
            {dst_port, Dst_port},
            {inet_version, Version}
           ],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop(Worker_ref) ->
    gen_server:start_link(Worker_ref).

% Internal Functions

% Behaviour callbacks

init([{tracker_srv, Server}, {socket, Socket}, {address, Address},
      {src_port, Src_port}, {dst_port, Dst_port},
      {inet_version, Version}]) ->
    case gen_udp:controlling_process(Socket, self()) of
        ok ->
            lager:debug("transfered ownership of socket"),
            {ok, Transaction_id, Connect_req} = ?TRACKER_UDP:connect_req(),

            case gen_udp:send(Socket, Address, Dst_port, Connect_req) of
                ok ->
                    Data = #state{socket = Socket,
                                  inet_version = Version,
                                  address = Address,
                                  ready = false,
                                  src_port = Src_port,
                                  dst_port = Dst_port},

                    {ok, Data, hibernate};
                {error, Reason} ->
                    lager:error("tracker worker failed to send connect request: '~w'", [Reason]),
                    {stop, "Failed to open UDP socket"}
            end;
        {error, Reason} ->
            lager:error("failed to transfer socket ownership to tracker worker"),
            {error, Reason}
    end.

handle_call(Request, From, State) ->
    lager:error("Unhandled call from '~w' '~w'", [From, Request]),
    {stop, "Unhandled call", State}.

handle_cast({announce, _From, _Args}, State) ->
    {noreply, State, hibernate};
handle_cast(Request, State) ->
    lager:error("Unhandled cast '~w'", [Request]),
    {stop, "Unhandled cast", State}.

handle_info({udp, _Socket, _IP, _InPortNo, Packet}, State) ->
    case ?TRACKER_UDP:parse_res(Packet, State#state.inet_version) of
        {ok, {error_res, Data}} ->
            [{transaction_id, Transaction_id}| Rest] = Data,
            % TODO remove transaction id entry from map and update state
            #{Transaction_id => {Requester, Type}} = State#state.transactions,
            Requester ! {tracker_udp, {error, Type}};
        {ok, {_, Data}} ->
            [{transaction_id, Transaction_id}| Rest] = Data,
            % TODO remove transaction id entry from map and update state
            #{Transaction_id => {Requester, Type}} = State#state.transactions,
            Requester ! {tracker_udp, {ok, Type, Rest}}
        {error, Bad_res} ->
            lager:error("failed to parse response '~w'", Bad_res)
    end,
handle_info({udp, _Socket, _IP, _InPortNo, _AncData, _Packet}, _State) ->
    lager:error("Unhandled UDP configuration AncData"),
    ok;
handle_info(Info, State) ->
    lager:error("Unhandled info '~w'", Info),
    {stop, "Unhandled info", State}.


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

% Implementation of the UDP Torrent tracker protocol.
% Specification:
% http://bittorrent.org/beps/bep_0015.html
-module(ertorrent_tracker_udp).

-export([
         connect_req/0,
         connect_res/1,
         announce_req/1,
         announce_res/1,
         scrape_req/1,
         scrape_res/1,
         parse_res/2,
         parse_req/1,
         error_res/1,
         timeout/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("ertorrent_modules.hrl").
-include("ertorrent_tracker.hrl").

-define(UDP_TRACKER_TIMEOUT_MAX, 8).
-define(UDP_TRACKER_TIMEOUT(X), (15 * math:pow(2, X))).

-define(UDP_TRACKER_MAGIC, 16#41727101980).

-define(CONNECT_REQ, <<Protocol_id:64/integer, % 0x41727101980, magic constant
                       0:32/integer, % 0
                       Transaction_id:32/integer>>).

-define(CONNECT_RES, <<0:32/integer, % 0
                       Transaction_id:32/bitstring,
                       Connection_id:64/bitstring>>).

-define(REQ_COMMON, <<Connection_id:64/bitstring,
                      Req_id:32/integer,
                      Transaction_id:32/bitstring,
                      Rest/binary>>).

-define(ANNOUNCE_REQ, <<Info_hash:160/bitstring,
                        Peer_id:160/bitstring,
                        Downloaded:64/integer,
                        Left:64/integer,
                        Uploaded:64/integer,
                        Event:32/integer,
                        IPv4:32/integer,
                        Key:32/integer,
                        Num_want:32/signed-integer,
                        Port:16/integer>>).

-define(ANNOUNCE_FREQ, <<Connection_id:64/bitstring,
                         1:32/integer, % 1
                         Transaction_id:32/bitstring,
                         Info_hash:160/bitstring,
                         Peer_id:160/bitstring,
                         Downloaded:64/integer,
                         Left:64/integer,
                         Uploaded:64/integer,
                         Event:32/integer,
                         IPv4:32/integer,
                         Key:32/integer,
                         Num_want:32/signed-integer,
                         Port:16/integer>>).

-define(ANNOUNCE_RES, <<1:32/integer, % 1
                        Transaction_id:32/bitstring,
                        Interval:32/integer,
                        Leechers:32/integer,
                        Seeders:32/integer,
                        Peers/binary>>).

-define(SCRAPE_REQ, <<Info_hashes/binary>>).

-define(SCRAPE_FREQ, <<Connection_id:64/bitstring,
                      2:32/integer, % 2
                      Transaction_id:32/bitstring,
                      Info_hashes/binary>>).

-define(SCRAPE_RES, <<2:32/integer, % 2
                      Transaction_id:32/bitstring,
                      Scrape_data/binary>>).

-define(ERROR_RES, <<3:32/integer, % 3
                     Transaction_id:32/bitstring,
                     Message/binary>>).

parse_info_hashes(Info_hashes) when is_binary(Info_hashes) ->
    {ok, [ Info_hash || <<Info_hash:160/bitstring>> <= Info_hashes]}.

parse_scrape_data(Scrape_data) when is_binary(Scrape_data) ->
    {ok, [{Seeders, Completed, Leechers} || <<Seeders:32/integer,
                                              Completed:32/integer,
                                              Leechers:32/integer>> <=
                                            Scrape_data]}.

gen_transaction_id() ->
    crypto:strong_rand_bytes(4).

connect_req() ->
    Protocol_id = ?UDP_TRACKER_MAGIC,
    % Using crypto:strong_rand_bytes/1 for now since it takes number of bytes
    % as argument. If the use of this function results in performance issues
    % later on, consider writing a weaker version since the cryptographic
    % strength is of no relevance here, just convenience.
    Transaction_id = gen_transaction_id(),
    Req = <<Protocol_id:64, 0:32, Transaction_id/binary>>,
    {ok, Transaction_id, Req}.

connect_res(Transaction_id) when is_binary(Transaction_id) andalso
                                 byte_size(Transaction_id) == 4 ->
    Connection_id = crypto:strong_rand_bytes(8),
    {ok, Connection_id, ?CONNECT_RES}.

announce_req(#'tracker.udp.msg.announce_req'{connection_id = Connection_id,
                                             transaction_id = Transaction_id,
                                             info_hash = Info_hash,
                                             peer_id = Peer_id,
                                             downloaded = Downloaded,
                                             left = Left,
                                             uploaded = Uploaded,
                                             event = Event,
                                             ipv4 = IPv4,
                                             key = Key,
                                             num_want = Num_want,
                                             port = Port}) ->
    {ok, ?ANNOUNCE_FREQ}.

announce_res(#'tracker.udp.msg.announce_res'{transaction_id = Transaction_id,
                                             interval = Interval,
                                             leechers = Leechers,
                                             seeders = Seeders,
                                             peers = Peers_in,
                                             peers6 = undefined}) ->
    Peers2 = [<<A/integer, B/integer, C/integer, D/integer, Port:16/integer>>
              || #'tracker.peer'{ip = {A, B, C, D}, port = Port} <- Peers_in],

    Peers = iolist_to_binary(Peers2),
    {ok, ?ANNOUNCE_RES};
announce_res(#'tracker.udp.msg.announce_res'{transaction_id = Transaction_id,
                                             interval = Interval,
                                             leechers = Leechers,
                                             seeders = Seeders,
                                             peers = undefined,
                                             peers6 = Peers6}) ->
    Peers2 = [<<A:16/integer,
                B:16/integer,
                C:16/integer,
                D:16/integer,
                E:16/integer,
                F:16/integer,
                G:16/integer,
                H:16/integer,
                Port:16/integer>> ||
              #'tracker.peer6'{ip = {A, B, C, D, E, F, G, H},
                               port = Port} <- Peers6],
    Peers = iolist_to_binary(Peers2),
    {ok, ?ANNOUNCE_RES}.

scrape_req(#'tracker.udp.msg.scrape_req'{connection_id = Connection_id,
                                         transaction_id = Transaction_id,
                                         info_hashes = Info_hash_list}) ->
    Info_hashes = iolist_to_binary(Info_hash_list),
    case binary:referenced_byte_size(Info_hashes) rem 20 of
        0 ->
            {ok, ?SCRAPE_FREQ};
        _ ->
            {error, invalid_size}
    end.

scrape_res(#'tracker.udp.msg.scrape_res'{transaction_id = Transaction_id,
                                         data = Data}) ->
    Stats_io = [<<Seeders:32/integer, Completed:32/integer,
                  Leechers:32/integer>>
                || {Seeders, Completed, Leechers} <- Data],
    Scrape_data = iolist_to_binary(Stats_io),
    {ok, ?SCRAPE_RES}.

error_res(#'tracker.udp.msg.error_res'{transaction_id = Transaction_id,
                                       message = Message}) ->
    {ok, ?ERROR_RES}.

parse_res(?ANNOUNCE_RES, inet) ->
    {ok, IPv4_peers} = ?BINARY:parse_peers4(Peers),

    {ok, #'tracker.udp.msg.announce_res'{transaction_id = Transaction_id,
                                         interval = Interval,
                                         leechers = Leechers,
                                         seeders = Seeders,
                                         peers = IPv4_peers}};
parse_res(?ANNOUNCE_RES, inet6) ->
    {ok, IPv6_peers} = ?BINARY:parse_peers6(Peers),

    {ok, #'tracker.udp.msg.announce_res'{transaction_id = Transaction_id,
                                         interval = Interval,
                                         leechers = Leechers,
                                         seeders = Seeders,
                                         peers6 = IPv6_peers}};
parse_res(?CONNECT_RES, _IP_fam) ->
    {ok, #'tracker.udp.msg.connect_res'{transaction_id = Transaction_id,
                                        connection_id = Connection_id}};
parse_res(?SCRAPE_RES, _IP_fam) ->
    {ok, Scrape_data_list} = parse_scrape_data(Scrape_data),
    {ok, #'tracker.udp.msg.scrape_res'{transaction_id = Transaction_id,
                                       data = Scrape_data_list}};
parse_res(?ERROR_RES, _IP_fam) ->
    {ok, #'tracker.udp.msg.error_res'{transaction_id = Transaction_id,
                                      message = Message}};
parse_res(Bad_res, _IP_fam) ->
    {error, Bad_res}.

handle_req(R = #'tracker.udp.msg.announce_req'{}, ?ANNOUNCE_REQ) ->
    {ok, R#'tracker.udp.msg.announce_req'{info_hash = Info_hash,
                                          peer_id = Peer_id,
                                          downloaded = Downloaded,
                                          left = Left,
                                          uploaded = Uploaded,
                                          event = Event,
                                          ipv4 = IPv4,
                                          key = Key,
                                          num_want = Num_want,
                                          port = Port}};
handle_req(R = #'tracker.udp.msg.scrape_req'{}, ?SCRAPE_REQ) ->
    {ok, Info_hashes_t} = parse_info_hashes(Info_hashes),
    {ok, R#'tracker.udp.msg.scrape_req'{info_hashes = Info_hashes_t}}.

parse_req(Req) when byte_size(Req) /= 16 ->
    ?REQ_COMMON = Req,
    case Req_id of
        1 ->
            handle_req(#'tracker.udp.msg.announce_req'{connection_id = Connection_id,
                                                       transaction_id = Transaction_id},
                                                       Rest);
        2 ->
            handle_req(#'tracker.udp.msg.scrape_req'{connection_id = Connection_id,
                                                     transaction_id = Transaction_id},
                                                     Rest)
    end;
parse_req(?CONNECT_REQ) ->
    {ok, #'tracker.udp.msg.connect_req'{protocol_id = Protocol_id,
                                        transaction_id = Transaction_id}}.

timeout(Transaction_id, Attempts) ->
    Seconds = trunc(15 * math:pow(2, Attempts)),
    erlang:send_after(Seconds,
                      self(),
                      #'tracker.udp.transaction_expired'{transaction_id = Transaction_id}).

%%%% UNIT TESTS %%%%

-ifdef(EUNIT).
-define(TEST_CONN_ID, <<195,125,28,4,116,86,94,10>>).
-define(TEST_TRAN_ID, <<0,41,56,204>>).
-define(TEST_INFO_HASH, <<"c12fe1c06bba254a9dc9">>).
-define(TEST_PEER_ID, <<"ET001-abcdefghijklmn">>).

connect_req_test() ->
    {ok, ID, Req} = connect_req(),
    <<T_id:32/integer>> = ID,

    {ok, #'tracker.udp.msg.connect_req'{protocol_id = Protocol_id,
                                        transaction_id = Transaction_id}} = parse_req(Req),

    ?assertEqual(Protocol_id, ?UDP_TRACKER_MAGIC),
    ?assertEqual(Transaction_id, T_id).

connect_res_test() ->
    Transaction_id = gen_transaction_id(),
    {ok, Connection_id, Msg} = connect_res(Transaction_id),

    {ok, #'tracker.udp.msg.connect_res'{transaction_id = Transaction_id2,
                                        connection_id = Connection_id2}} = parse_res(Msg, ipv4_or_ipv6),

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual(Connection_id, Connection_id2).

announce_req_test() ->
    Args = #'tracker.udp.msg.announce_req'{connection_id = ?TEST_CONN_ID,
                                           transaction_id = ?TEST_TRAN_ID,
                                           info_hash = ?TEST_INFO_HASH,
                                           peer_id = ?TEST_PEER_ID,
                                           downloaded = 1,
                                           left = 2,
                                           uploaded = 3,
                                           event = 2,
                                           ipv4 = 0,
                                           key = 1,
                                           num_want = -1,
                                           port = 1337},

    {ok, Msg} = announce_req(Args),
    {ok, Req} = parse_req(Msg),
    ?assertEqual(Args, Req).

announce_res_ipv4_test() ->
    Peers = [
             #'tracker.peer'{ip={127,0,0,1}, port = 42},
             #'tracker.peer'{ip={192,168,0,1}, port = 45}
            ],
    Args = #'tracker.udp.msg.announce_res'{transaction_id = ?TEST_TRAN_ID,
                                           interval = 52,
                                           leechers = 654,
                                           seeders = 234799,
                                           peers = Peers},
    {ok, Msg} = announce_res(Args),
    {ok, Res} = parse_res(Msg, inet),

    ?assertEqual(Args, Res).

announce_res_ipv6_test() ->
    Peers = [
             #'tracker.peer6'{ip={0,0,0,0,0,0,0,0}, port = 42},
             #'tracker.peer6'{ip={8193,3512,0,0,0,65280,66,33577}, port = 45}
            ],
    Args = #'tracker.udp.msg.announce_res'{transaction_id = ?TEST_TRAN_ID,
                                           interval = 52,
                                           leechers = 654,
                                           seeders = 234799,
                                           peers6 = Peers},


    {ok, Msg} = announce_res(Args),
    {ok, Res} = parse_res(Msg, inet6),
    ?assertEqual(Args, Res).

scrape_req_test() ->
    Args = #'tracker.udp.msg.scrape_req'{connection_id = ?TEST_CONN_ID,
                                         transaction_id = ?TEST_TRAN_ID,
                                         info_hashes = [?TEST_INFO_HASH,
                                                        ?TEST_INFO_HASH,
                                                        ?TEST_INFO_HASH]},

    {ok, Msg} = scrape_req(Args),
    {ok, Req} = parse_req(Msg),
    ?assertEqual(Args, Req).

% Invalid hash based on length
scrape_req1_test() ->
    Args = #'tracker.udp.msg.scrape_req'{connection_id = ?TEST_CONN_ID,
                                         transaction_id = ?TEST_TRAN_ID,
                                         info_hashes = [?TEST_INFO_HASH,
                                                        <<"not a valid hash">>,
                                                        ?TEST_INFO_HASH]},

    Msg = scrape_req(Args),
    ?assertEqual({error, invalid_size}, Msg).

scrape_res_test() ->
    Args = #'tracker.udp.msg.scrape_res'{transaction_id = ?TEST_TRAN_ID,
                                         data = [{1, 2, 3}, {4, 5, 6}]},

    {ok, Msg} = scrape_res(Args),
    {ok, Res} = parse_res(Msg, ipv4_or_ipv6),
    ?assertEqual(Args, Res).

error_res_test() ->
    Args = #'tracker.udp.msg.error_res'{transaction_id = ?TEST_TRAN_ID,
                                        message = <<"Houston we got a problem">>},
    {ok, Msg} = error_res(Args),
    {ok, Res} = parse_res(Msg, ipv4_or_ipv6),
    ?assertEqual(Args, Res).

-endif.

% Implementation of the UDP Torrent tracker protocol.
% Specification:
% http://bittorrent.org/beps/bep_0015.html
-module(ertorrent_tracker_udp).

-export([
         connect_req/0,
         connect_res/1,
         announce_req/2,
         announce_res_ipv4/2,
         announce_res_ipv6/2,
         scrape_req/2,
         scrape_res/2,
         parse_res/2,
         parse_req/1,
         error_res/2
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("ertorrent_modules.hrl").

-define(UDP_TRACKER_TIMEOUT_MAX, 8).
-define(UDP_TRACKER_TIMEOUT(X), (15 * math:pow(2, X))).

-define(UDP_TRACKER_MAGIC, 16#41727101980).

-define(CONNECT_REQ, <<Protocol_id:64/integer, % 0x41727101980, magic constant
                       0:32/integer, % 0
                       Transaction_id:32/integer>>).

-define(CONNECT_RES, <<0:32/integer, % 0
                       Transaction_id:32/bitstring,
                       Connection_id:64/bitstring>>).

-define(REQ_COMMON, <<Connection_id:64/integer,
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

-define(ANNOUNCE_FREQ, <<Connection_id:64/integer,
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

-define(SCRAPE_FREQ, <<Connection_id:64/integer,
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
    {ok, {info_hashes, [ Info_hash || <<Info_hash:160/bitstring>> <= Info_hashes]}}.

parse_scrape_data(Scrape_data) when is_binary(Scrape_data) ->
    {ok, [{Seeders, Completed, Leechers} || <<Seeders:32/integer,
                                                            Completed:32/integer,
                                                            Leechers:32/integer>>
                                                          <= Scrape_data]}.

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

announce_req(Connection_id, [{info_hash, Info_hash},
                             {peer_id, Peer_id},
                             {downloaded, Downloaded},
                             {left, Left},
                             {uploaded, Uploaded},
                             {event, Event},
                             {ip_addr, IPv4},
                             {key, Key},
                             {num_want, Num_want},
                             {port, Port}
                            ]) when is_binary(Info_hash) andalso
                                    is_binary(Peer_id) ->
    Transaction_id = gen_transaction_id(),
    {ok, Transaction_id, ?ANNOUNCE_FREQ}.

announce_res_ipv4(Transaction_id, [{interval, Interval}, {leechers, Leechers},
                              {seeders, Seeders}, {peers, Peers_in}]) ->
    Peers2 = [<<A/integer, B/integer, C/integer, D/integer, Port:16/integer>>
              || {peer4, {ipv4, {A, B, C, D}}, {port, Port}} <- Peers_in],
    Peers = iolist_to_binary(Peers2),
    {ok, ?ANNOUNCE_RES}.

announce_res_ipv6(Transaction_id, [{interval, Interval}, {leechers, Leechers},
                              {seeders, Seeders}, {peers, Peers_in}]) ->
    Peers2 = [<<A:16/integer,
                B:16/integer,
                C:16/integer,
                D:16/integer,
                E:16/integer,
                F:16/integer,
                G:16/integer,
                H:16/integer,
                Port:16/integer>> || {peer6,
                                      {ipv6, {A, B, C, D, E, F, G, H}},
                                      {port, Port}} <- Peers_in],
    Peers = iolist_to_binary(Peers2),
    {ok, ?ANNOUNCE_RES}.

scrape_req(Connection_id, Info_hash_list) ->
    Info_hashes = iolist_to_binary(Info_hash_list),
    case binary:referenced_byte_size(Info_hashes) rem 20 of
        0 ->
            Transaction_id = gen_transaction_id(),
            {ok, Transaction_id, ?SCRAPE_FREQ};
        _ ->
            {error, "Invalid size"}
    end.

scrape_res(Transaction_id, Stats) ->
    Stats_io = [<<Seeders:32/integer, Completed:32/integer,
                  Leechers:32/integer>>
                || {Seeders, Completed, Leechers} <- Stats],
    Scrape_data = iolist_to_binary(Stats_io),
    {ok, ?SCRAPE_RES}.

error_res(Transaction_id, Message_in) ->
    Message = list_to_binary(Message_in),
    {ok, ?ERROR_RES}.

parse_res(?ANNOUNCE_RES, inet) ->
    Transaction_id_t = {transaction_id, Transaction_id},
    Interval_t = {interval, Interval},
    Leechers_t = {leechers, Leechers},
    Seeders_t = {seeders, Seeders},

    {ok, IPv4_peers} = ?BINARY:parse_peers4(Peers),

    Peers_t = {peers, IPv4_peers},

    {ok, {announce_res, [Transaction_id_t, Interval_t, Leechers_t, Seeders_t,
                         Peers_t]}};
parse_res(?ANNOUNCE_RES, inet6) ->
    Transaction_id_t = {transaction_id, Transaction_id},
    Interval_t = {interval, Interval},
    Leechers_t = {leechers, Leechers},
    Seeders_t = {seeders, Seeders},

    {ok, IPv6_peers} = ?BINARY:parse_peers6(Peers),

    Peers_t = {peers, IPv6_peers},

    {ok, {announce_res, [Transaction_id_t, Interval_t, Leechers_t, Seeders_t,
                         Peers_t]}};
parse_res(?CONNECT_RES, _IP_fam) ->
    Transaction_id_t = {transaction_id, Transaction_id},
    Connection_id_t = {connection_id, Connection_id},
    {ok, {connect_res, [Transaction_id_t, Connection_id_t]}};
parse_res(?SCRAPE_RES, _IP_fam) ->
    Transaction_id_t = {transaction_id, Transaction_id},
    {ok, Scrape_data_list} = parse_scrape_data(Scrape_data),
    Scrape_data_t = {scrape_data, Scrape_data_list},
    {ok, {scrape_res, [Transaction_id_t, Scrape_data_t]}};
parse_res(?ERROR_RES, _IP_fam) ->
    Transaction_id_t = {transaction_id, Transaction_id},
    Message_t = {message, binary_to_list(Message)},
    {ok, {error_res, [Transaction_id_t, Message_t]}};
parse_res(Bad_res, _IP_fam) ->
    {error, Bad_res}.

handle_req(1, ?ANNOUNCE_REQ) ->
    {ok, {announce_req, [{info_hash, Info_hash}, {peer_id, Peer_id},
                         {downloaded, Downloaded}, {left, Left},
                         {uploaded, Uploaded}, {event, Event}, {ipv4, IPv4},
                         {key, Key}, {num_want, Num_want}, {port, Port}]}};
handle_req(2, ?SCRAPE_REQ) ->
    {ok, Info_hashes_t} = parse_info_hashes(Info_hashes),
    {ok, {scrape_req, [Info_hashes_t]}}.

parse_req(Req) when byte_size(Req) /= 16 ->
    ?REQ_COMMON = Req,
    Connection_id_t = {connection_id, Connection_id},
    Transaction_id_t = {transaction_id, Transaction_id},
    {ok, {Type, TypeData}} = handle_req(Req_id, Rest),
    {ok, {Type, [Connection_id_t, Transaction_id_t| TypeData]}};
parse_req(Req) ->
    ?CONNECT_REQ = Req,
    Protocol_id_t = {protocol_id, Protocol_id},
    Transaction_id_t = {transaction_id, Transaction_id},
    {ok, {connect_req, {Protocol_id_t, Transaction_id_t}}}.


%%%% UNIT TESTS %%%%

-ifdef(EUNIT).
-define(TEST_INFO_HASH, <<"c12fe1c06bba254a9dc9">>).
-define(TEST_PEER_ID, <<"ET001-abcdefghijklmn">>).

connect_req_test() ->
    {ok, ID, Req} = connect_req(),
    <<T_id:32/integer>> = ID,

    {ok, Parse_res} = parse_req(Req),
    {connect_req,
     {{protocol_id, Protocol_id},
      {transaction_id, Transaction_id}}} = Parse_res,

    ?assertEqual(Protocol_id, ?UDP_TRACKER_MAGIC),
    ?assertEqual(Transaction_id, T_id).

connect_res_test() ->
    Transaction_id = gen_transaction_id(),
    {ok, Connection_id, Msg} = connect_res(Transaction_id),

    {ok, {connect_res, MsgCont}} = parse_res(Msg, ipv4_or_ipv6),
    [{transaction_id, Transaction_id2},
     {connection_id, Connection_id2}] = MsgCont,

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual(Connection_id, Connection_id2).

announce_req_test() ->
    Req_args = [
                {info_hash, ?TEST_INFO_HASH},
                {peer_id, ?TEST_PEER_ID},
                {downloaded, 1},
                {left, 2},
                {uploaded, 3},
                {event, 2},
                {ip_addr, 0},
                {key, 1},
                {num_want, -1},
                {port, 1337}
               ],
    {ok, ID, Req} = announce_req(12345678, Req_args),
    ?assertEqual(binary:referenced_byte_size(Req), 98),

    {ok, Parse_res} = parse_req(Req),
    {announce_req, [
        {connection_id, Connection_id},
        {transaction_id, Transaction_id},
        {info_hash, Info_hash},
        {peer_id, Peer_id},
        {downloaded, Downloaded},
        {left, Left},
        {uploaded, Uploaded},
        {event, Event},
        {ipv4, IPv4},
        {key, Key},
        {num_want, Num_want},
        {port, Port}]} = Parse_res,

    ?assertEqual(12345678, Connection_id),
    ?assertEqual(ID, Transaction_id),
    ?assertEqual(?TEST_INFO_HASH, Info_hash),
    ?assertEqual(?TEST_PEER_ID, Peer_id),
    ?assertEqual(1, Downloaded),
    ?assertEqual(2, Left),
    ?assertEqual(3, Uploaded),
    ?assertEqual(2, Event),
    ?assertEqual(0, IPv4),
    ?assertEqual(1, Key),
    ?assertEqual(-1, Num_want),
    ?assertEqual(1337, Port).

announce_res_ipv4_test() ->
    Transaction_id = gen_transaction_id(),
    Interval_t = {interval, 52},
    Leechers_t = {leechers, 654},
    Seeders_t = {seeders, 234799},
    Peers_in = [{peer4, {ipv4, {127,0,0,1}}, {port, 42}},
                {peer4, {ipv4, {192,168,0,1}}, {port, 45}}],
    Peers_t = {peers, Peers_in},

    Args = [Interval_t, Leechers_t, Seeders_t, Peers_t],

    {ok, Msg} = announce_res_ipv4(Transaction_id, Args),
    {ok, {announce_res, MsgCont}} = parse_res(Msg, inet),

    [{transaction_id, Transaction_id2},
     {interval, Interval},
     {leechers, Leechers},
     {seeders, Seeders},
     {peers, Peers}] = MsgCont,

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual(52, Interval),
    ?assertEqual(654, Leechers),
    ?assertEqual(234799, Seeders),
    ?assertEqual(Peers_in, Peers).

announce_res_ipv6_test() ->
    Transaction_id = gen_transaction_id(),
    Interval_t = {interval, 52},
    Leechers_t = {leechers, 654},
    Seeders_t = {seeders, 234799},
    Peers_in = [{peer6,
                 {ipv6, {0,0,0,0,0,0,0,0}},
                 {port, 42}},
                        {peer6,
                 {ipv6, {8193,3512,0,0,0,65280,66,33577}},
                 {port, 45}}],
    Peers_t = {peers, Peers_in},

    Args = [Interval_t, Leechers_t, Seeders_t, Peers_t],

    {ok, Msg} = announce_res_ipv6(Transaction_id, Args),
    {ok, {announce_res, MsgCont}} = parse_res(Msg, inet6),

    [{transaction_id, Transaction_id2},
     {interval, Interval},
     {leechers, Leechers},
     {seeders, Seeders},
     {peers, Peers}] = MsgCont,

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual(52, Interval),
    ?assertEqual(654, Leechers),
    ?assertEqual(234799, Seeders),
    ?assertEqual(Peers_in, Peers).

scrape_req_p0_test() ->
    Req_args = [?TEST_INFO_HASH,
                ?TEST_INFO_HASH,
                ?TEST_INFO_HASH],

    {ok, ID, Req} = scrape_req(12345678, Req_args),
    ?assertEqual(binary:referenced_byte_size(Req),
                 16+20*length(Req_args)),

    {ok, Res} = parse_req(Req),
    {scrape_req, [
        {connection_id, Connection_id},
        {transaction_id, Transaction_id},
        {info_hashes, Info_hashes}]} = Res,

    ?assertEqual(12345678, Connection_id),
    ?assertEqual(ID, Transaction_id),
    ?assertEqual(Req_args, Info_hashes).

scrape_req_n0_test() ->
    Req_args = [?TEST_INFO_HASH,
                <<"not a valid hash">>,
                ?TEST_INFO_HASH],

    {Res, ResMsg} = scrape_req(12345678, Req_args),
    ?assertEqual(error, Res),
    ?assertEqual("Invalid size", ResMsg).

scrape_res_test() ->
    Transaction_id = gen_transaction_id(),
    Args = [{1, 2, 3}, {4, 5, 6}],

    {ok, Msg} = scrape_res(Transaction_id, Args),

    {ok, {scrape_res, MsgCont}} = parse_res(Msg, ipv4_or_ipv6),

    [{transaction_id, Transaction_id2},
     {scrape_data, Scrape_data}] = MsgCont,

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual(Args, Scrape_data).

error_res_test() ->
    Transaction_id = gen_transaction_id(),

    {ok, Msg} = error_res(Transaction_id, "Houston we got a problem"),

    {ok, {error_res, MsgCont}} = parse_res(Msg, ipv4_or_ipv6),

    [{transaction_id, Transaction_id2},
     {message, Message}] = MsgCont,

    ?assertEqual(Transaction_id, Transaction_id2),
    ?assertEqual("Houston we got a problem", Message).
    
-endif.

-module(http_announce).

-include("metainfo.hrl").
-include("announce_response.hrl").
-include("peer.hrl").

-export([do_announce/1]).

do_announce(Metainfo) ->
    #metainfo{announce=Announce_address,
              info_hash=Info_hash,
              info=Info} = Metainfo,
    #info{length=Length} = Info,
    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    %Replacing reserved characters
    Peer_id_encoded = edoc_lib:escape_uri(Peer_id),
    %compact=1 is required by BEP 23:
    %http://www.bittorrent.org/beps/bep_0023.html
    Request = lists:concat([binary_to_list(Announce_address), '?',
                            "info_hash=", Info_hash,
                            "&peer_id=", Peer_id_encoded,
                            "&port=6882",
                            "&uploaded=0",
                            "&downloaded=0",
                            "&left=", Length,
                            "&event=started",
                            "&compact=1"]),
    ok = inets:start(),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Request, []}, [], [{sync, true}]),
    ok = inets:stop(),
    {ok, {{dict, Decoded}, _}} = bencode:decode(Body),
    parse_response(Decoded, #announce_response{}).

parse_response([], Record) ->
    {ok, Record};
parse_response([{<<"complete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{complete=Value},
    parse_response(Tail, New_record);
parse_response([{<<"incomplete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{incomplete=Value},
    parse_response(Tail, New_record);
parse_response([{<<"interval">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{interval=Value},
    parse_response(Tail, New_record);
parse_response([{<<"peers">>, Value}|Tail], Record) ->
    {ok, Peers} = parse_peers(Value, []),
    New_record = Record#announce_response{peers=Peers},
    parse_response(Tail, New_record).

parse_peers(<<>>, Acc) ->
    {ok, Acc};
parse_peers(<<Ip:4/binary-unit:8, Port:2/binary-unit:8, Tail/binary>>, Acc) ->
    Peer = #peer{ip=binary_to_list(Ip), port=binary_to_list(Port)},
    parse_peers(Tail, [Peer|Acc]).

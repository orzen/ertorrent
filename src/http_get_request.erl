%%%%%%---------------------------------------------------------------------
%%%%%% Description module http_get_request
%%%%%%---------------------------------------------------------------------
%%%%%% This module contains everything necessary to perform a HTTP tracker
%%%%%% GET request and handle its response. This module supports both basic
%%%%%% and compact mode (explained in BEP 23).
%%%%%%
%%%%%% Links:
%%%%%% http://bittorrent.org/beps/bep_0003.html
%%%%%% http://www.bittorrent.org/beps/bep_0023.html
%%%%%%---------------------------------------------------------------------
%%%%%% Exports
%%%%%%---------------------------------------------------------------------
%%%%%% send(Metainfo, Port, Uploaded, Downloaded, Event)
%%%%%%   Returns the response of a performed GET request with the given
%%%%%%   metainfo.
%%%%%%---------------------------------------------------------------------
-module(http_get_request).

-include("metainfo.hrl").
-include("announce_response.hrl").
-include("peer.hrl").

-export([send/5]).

send(Metainfo, Port, Uploaded, Downloaded, Event) ->
    #metainfo{announce=Announce_address,
              info_hash=Info_hash,
              info=Info} = Metainfo,
    #info{length=Length} = Info,
    Peer_id_encoded = edoc_lib:escape_uri(version:version(peer_id)),
    Basic_request = form_request(Announce_address,
                                 Info_hash,
                                 Peer_id_encoded,
                                 Port,
                                 Uploaded,
                                 Downloaded,
                                 Length,
                                 Event,
                                 false),
    {ok, Code, Basic_response} = send_request(Basic_request),
    erlang:display(Basic_request),
    if
        Code =:= 200 ->
            erlang:display("BASIC REQUEST"),
            {ok, Response} = parse_basic_response(Basic_response);
        Code =:= 400 ->
            erlang:display("COMPACT REQUEST"),
            Compact_request = form_request(Announce_address,
                                           Info_hash,
                                           Peer_id_encoded,
                                           Port,
                                           Uploaded,
                                           Downloaded,
                                           Length,
                                           Event,
                                           true),
            {ok, 200, Compact_response} = send_request(Compact_request),
            {ok, Response} = parse_compact_response(Compact_response)
    end,
    Response.

send_request(Request) ->
    inets:start(),
    try
        {ok, {{_, Code, _}, _, Response}} = httpc:request(get,
                                                          {Request,
                                                           [{"Accept",
                                                             "text/plain"}]},
                                                          [],
                                                          [{sync, true},
                                                           {headers_as_is, true}]),
        {ok, Code, Response}
    catch
        Exception:Reason -> {error, Exception, Reason}
    after
        inets:stop()
    end.

form_request(Announce_address, Info_hash, Peer_id_encoded, Port, Uploaded, Downloaded, Length, Event, Compact) ->
    Basic_request = lists:concat([binary_to_list(Announce_address), "?",
                                 "info_hash=", Info_hash,
                                 "&peer_id=", Peer_id_encoded,
                                 "&port=", Port,
                                 "&uploaded=", Uploaded,
                                 "&downloaded=", Downloaded,
                                 "&left=", Length,
                                 "&event=", Event]),
    case Compact of
        true ->
            lists:concat([Basic_request, "&compact=1"]);
        false ->
            Basic_request
    end.

decode_response(Response) ->
    try
        {ok, {dict, Response_decoded}} = bencode:decode(Response),
        {ok, Response_decoded}
    catch
        Exception:Reason -> {Exception, Reason}
    end.

parse_basic_response(Response_encoded) ->
    {ok, Response_decoded} = decode_response(Response_encoded),
    parse_decoded_basic_response(Response_decoded, #announce_response{}).

parse_decoded_basic_response([], Record) ->
    {ok, Record};
parse_decoded_basic_response([{<<"complete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{complete=Value},
    parse_decoded_basic_response(Tail, New_record);
parse_decoded_basic_response([{<<"incomplete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{incomplete=Value},
    parse_decoded_basic_response(Tail, New_record);
parse_decoded_basic_response([{<<"interval">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{interval=Value},
    parse_decoded_basic_response(Tail, New_record);
parse_decoded_basic_response([{<<"peers">>, Value}|Tail], Record) ->
    {ok, Peers} = parse_basic_peers_list(Value, []),
    New_record = Record#announce_response{peers={Peers}},
    parse_decoded_basic_response(Tail, New_record).

parse_basic_peers_list(<<>>, Acc) ->
    {ok, Acc};
parse_basic_peers_list(<<Id:23/big-binary-unit:8,
                         Ip:259/big-binary-unit:8,
                         Port:7/big-binary-unit:8,
                         Tail/binary>>, Acc) ->
    Peer = #peer{type=basic,
                 id=bencode:decode(Id),
                 ip=bencode:decode(Ip),
                 port=bencode:decode(Port)},
    parse_basic_peers_list(Tail, [Peer|Acc]).

parse_compact_response(Response_encoded) ->
    {ok, Response_decoded} = decode_response(Response_encoded),
    parse_decoded_compact_response(Response_decoded, #announce_response{}).

parse_decoded_compact_response([], Record) ->
    {ok, Record};
parse_decoded_compact_response([{<<"complete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{complete=Value},
    parse_decoded_compact_response(Tail, New_record);
parse_decoded_compact_response([{<<"incomplete">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{incomplete=Value},
parse_decoded_compact_response(Tail, New_record);
parse_decoded_compact_response([{<<"interval">>, Value}|Tail], Record) ->
    New_record = Record#announce_response{interval=Value},
    parse_decoded_compact_response(Tail, New_record);
parse_decoded_compact_response([{<<"peers">>, Value}|Tail], Record) ->
    {ok, Peers} = parse_compact_peers_list(Value, []),
    New_record = Record#announce_response{peers={Peers}},
    parse_decoded_compact_response(Tail, New_record).

parse_compact_peers_list(<<>>, Acc) ->
    {ok, Acc};
parse_compact_peers_list(<<Ip:4/big-binary-unit:8,
                           Port:2/big-integer-unit:8,
                           Tail/binary>>, Acc) ->
    Peer = #peer{type=compact, ip=binary_to_list(Ip), port=Port},
    parse_compact_peers_list(Tail, [Peer|Acc]).

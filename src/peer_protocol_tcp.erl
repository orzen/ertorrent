-module(peer_protocol_tcp).

-export([msg_handshake/2,
         msg_keep_alive/0,
         msg_choke/0,
         msg_unchoke/0,
         msg_interested/0,
         msg_not_interested/0,
         msg_have/1,
         msg_bitfield/2,
         msg_request/3,
         msg_piece/4,
         msg_cancel/3,
         msg_port/1]).

-define(CHOKE, <<0:8>>).
-define(UNCHOKE, <<1:8>>).
-define(INTERESTED, <<2:8>>).
-define(NOT_INTERESTED, <<3:8>>).
-define(HAVE, <<4:8>>).
-define(BITFIELD, <<5:8>>).
-define(REQUEST, <<6:8>>).
-define(PIECE, <<7:8>>).
-define(CANCEL, <<8:8>>).
-define(PORT, <<9:8>>).

-type handshake()::<<19:32, "BitTorrent protocol", 0:64, Info_hash:160, Peer_id:160>>.
-type keepalive()::<<0:32>>.
-type choke()::<<1:32, ?CHOKE>>.
-type unchoke()::<<1:32, ?UNCHOKE>>.
-type interested()::<<1:32, ?INTERESTED>>.
-type not_interested()::<<1:32, ?NOT_INTERESTED>>.
-type have()::<<5:32, ?HAVE, Piece_hash:32>>.
% Remember Id represents one of the bytes of the bitfield length
-type bitfield()::<<Length:32, ?HAVE, Bitfield>>.
-type request()::<<13:32, ?REQUEST, Piece_hash:32, Begin:32, Length:32>>.
-type piece()::<<Length:32, ?PIECE, Piece_hash:32, Begin:32, Block:32>>.
-type cancel()::<<13:32, ?CANCEL, Piece_hash:32, Begin:32, Length:32>>.
-type port()::<<3:32, ?PORT, Listen_port:32>>.


parse_length_prefixed_value(Length, Bitstring) ->
    <<Value:Length/big-integer-unit:8, Rest>> = Bitstring,
    {ok, {Value, Rest}}.

parse_message(Bytestring) ->
    {ok, Message} = parse_message(Bytestring, []).
parse_message(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
parse_message(<<Length:32/big, Rest/big>>, Acc) ->
    case Length =:= 0 of
        true -> [0];
        false -> {ok, {Value, New_rest}} = parse_length_prefixed_value(Length, Rest),
                 parse_message(New_rest, [Value|Acc])
    end.

msg_handshake(Info_hash, Peer_id) ->
    {ok, <<19:32, "BitTorrent protocol", 0:64, Info_hash:160, Peer_id:160>>}.

msg_keep_alive() ->
    {ok, <<0:32>>}.

msg_choke() ->
    {ok, <<1:32, ?CHOKE:8>>}.

msg_unchoke() ->
    {ok, <<1:32, ?UNCHOKE:8>>}.

msg_interested() ->
    {ok, <<1:32, ?INTERESTED:8>>}.

msg_not_interested() ->
    {ok, <<1:32, ?NOT_INTERESTED:8>>}.

msg_have(Piece_index) ->
    {ok, <<5:32, ?HAVE:8, Piece_index>>}.

msg_bitfield(Bitfield_length, Bitfield) ->
    Length = 1 + Bitfield_length,
    {ok, <<Length:32, ?BITFIELD:8, Bitfield>>}.

msg_request(Length, Index, Begin) ->
    {ok, <<13:32, ?REQUEST:8, Index:32, Begin:32, Length:32>>}.

msg_piece(Piece_length, Index, Begin, Block) ->
    Length = 9 + Piece_length,
    {ok, <<Length:32, ?PIECE:8, Index:32, Begin:32, Block:32>>}.

msg_cancel(Index, Begin, Length) ->
    {ok, <<13:32, ?CANCEL:8, Index:32, Begin:32, Length:32>>}.

msg_port(Listen_port) ->
    {ok, <<3:32, ?PORT:8, Listen_port:32>>}.

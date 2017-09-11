-module(ertorrent_peer_tcp_protocol).

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

-include("ertorrent_peer_tcp_message_ids.hrl").

% TODO remove if not used
% parse_length_prefixed_value(Length, Bitstring) ->
%     <<Value:Length/big-integer-unit:8, Rest>> = Bitstring,
%     {ok, {Value, Rest}}.
% 
% parse_message(Bytestring) ->
%     parse_message(Bytestring, []).
% parse_message(<<>>, Acc) ->
%     {ok, lists:reverse(Acc)};
% parse_message(<<Length:32/big, Rest/big>>, Acc) ->
%     case Length =:= 0 of
%         true -> [0];
%         false -> {ok, {Value, New_rest}} = parse_length_prefixed_value(Length, Rest),
%                  parse_message(New_rest, [Value|Acc])
%     end.


% TODO: Get some statistics of most common message types to priorites the match
% below to improve the decode performance.
% decode_message(Message) ->
%     case Message of
%     end.

msg_handshake(Info_hash, Peer_id) ->
    {ok, <<19:32, "BitTorrent protocol", 0:64, Info_hash:160, Peer_id:160>>}.

msg_keep_alive() ->
    {ok, <<0:32>>}.

msg_choke() ->
    {ok, <<1:32, ?CHOKE>>}.

msg_unchoke() ->
    {ok, <<1:32, ?UNCHOKE>>}.

msg_interested() ->
    {ok, <<1:32, ?INTERESTED>>}.

msg_not_interested() ->
    {ok, <<1:32, ?NOT_INTERESTED>>}.

msg_have(Piece_index) ->
    {ok, <<5:32, ?HAVE, Piece_index>>}.

msg_bitfield(Bitfield_length, Bitfield) ->
    Length = 1 + Bitfield_length,
    {ok, <<Length:32, ?BITFIELD, Bitfield>>}.

msg_request(Index, Begin, Length) ->
    {ok, <<13:32, ?REQUEST, Index:32, Begin:32, Length:32>>}.

msg_piece(Block_size, Index, Begin, Block) ->
    Length = 9 + Block_size,
    {ok, <<Length:32, ?PIECE, Index:32, Begin:32, Block:32>>}.

msg_cancel(Index, Begin, Length) ->
    {ok, <<13:32, ?CANCEL, Index:32, Begin:32, Length:32>>}.

msg_port(Listen_port) ->
    {ok, <<3:32, ?PORT, Listen_port:32>>}.

%%%---------------------------------------------------------------------
%%% Description module peer_protocol_tcp
%%%---------------------------------------------------------------------
%%% This module provides utility functions to create and parse peer wire
%%% protocol messages over TCP.
%%%
%%% Links:
%%% http://bittorrent.org/beps/bep_0003.html
%%%---------------------------------------------------------------------
%%% Exports
%%%---------------------------------------------------------------------
%%% parse_message(Bitstring)
%%%   returns a list with the content of a peer message
%%%
%%% msg_handshake(Info_hash, Peer_id)
%%%   returns a handshake message based on the Info_hash and the Peer_id
%%%
%%% msg_keep_alive()
%%%
%%% msg_choke()
%%%
%%% msg_unchoke()
%%%
%%% msg_interested()
%%%
%%% msg_not_interested()
%%%
%%% msg_have(Piece_index)
%%%
%%% msg_bitfield(Bitfield_length, Bitfield)
%%%
%%% msg_request(Length, Index, Begin)
%%%
%%% msg_piece(Piece_length, Index, Begin, Block)
%%%
%%% msg_cancel(Index, Begin, Length)
%%%
%%% msg_port(Listen_port)
%%%
%%%---------------------------------------------------------------------

-module(peer_protocol_tcp).

%% Related to incoming messages
-export([parse_message/1]).

%% Related to outgoing messages
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

-include("peer_protocol_message_id.hrl").

parse_message(Message) ->
    case Message of
        <<0:32/big-binary>> ->
            {keep_alive};
        <<1:32/big-binary, ?CHOKE>> ->
            {choke};
        <<1:32/big-binary, ?UNCHOKE>> ->
            {unchoke};
        <<1:32/big-binary, ?INTERESTED>> ->
            {interested};
        <<1:32/big-binary, ?NOT_INTERESTED>> ->
            {not_interested};
        <<5:32/big-binary, ?HAVE, Piece_index>> ->
            {have, Piece_index};
        <<Length:32/big-binary, ?BITFIELD, Bitfield>> ->
            {bitfield, Length, <<Bitfield:Length/binary-unit:8>>};
        <<13:32/big-binary, ?REQUEST, Index:32, Begin:32, Length:32>> ->
            {request, Index, Begin, Length};
        <<Length:32/big-binary, ?PIECE, Index:32/integer, Begin:32/integer, Block/integer>> ->
            Block_length = Length - 9,
            {piece, Block_length, Index, Begin, Block};
        <<13:32/big-binary, ?CANCEL, Index:32, Begin:32, Length:32>> ->
            {cancel, Index, Begin, Length};
        <<3:32/big-binary, ?PORT, Listen_port/integer>> ->
            {port, Listen_port}
    end.

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

msg_request(Length, Index, Begin) ->
    {ok, <<13:32, ?REQUEST, Index:32, Begin:32, Length:32>>}.

msg_piece(Piece_length, Index, Begin, Block) ->
    Length = 9 + Piece_length,
    {ok, <<Length:32, ?PIECE, Index:32, Begin:32, Block:32>>}.

msg_cancel(Index, Begin, Length) ->
    {ok, <<13:32, ?CANCEL, Index:32, Begin:32, Length:32>>}.

msg_port(Listen_port) ->
    {ok, <<3:32, ?PORT, Listen_port:32>>}.

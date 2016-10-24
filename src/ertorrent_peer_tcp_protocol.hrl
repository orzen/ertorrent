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

-define(CHOKE, 0).
-define(UNCHOKE, 1).
-define(INTERESTED, 2).
-define(NOT_INTERESTED, 3).
-define(HAVE, 4).
-define(BITFIELD, 5).
-define(REQUEST, 6).
-define(PIECE, 7).
-define(CANCEL, 8).
-define(PORT, 9).
% Fast Extension
% http://www.bittorrent.org/beps/bep_0006.html
-define(SUGGEST_PIECE, 13).
-define(HAVE_ALL, 14).
-define(HAVE_NONE, 15).
-define(REJECT_PIECE, 16).
-define(ALLOWED_FAST, 17).

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
    %Flags = 16#0004000000000000,
    {ok, <<19:8/integer,
           "BitTorrent protocol"/integer,
           0:64/big,
           Info_hash:20/binary,
           Peer_id:20/binary>>}.

msg_keep_alive() ->
    {ok, <<0:32>>}.

msg_choke() ->
    {ok, <<1:32, ?CHOKE:8/big-integer>>}.

msg_unchoke() ->
    {ok, <<1:32, ?UNCHOKE:8/big-integer>>}.

msg_interested() ->
    {ok, <<1:32, ?INTERESTED:8/big-integer>>}.

msg_not_interested() ->
    {ok, <<1:32, ?NOT_INTERESTED:8/big-integer>>}.

msg_have(Piece_index) ->
    {ok, <<5:32, ?HAVE:8/big, Piece_index>>}.

send_keep_alive(Socket, Timer) ->
    TimerRef = erlang:send_after(Timer, self(), {keep_alive_internal_timeout}),

    {ok, Keep_alive} = msg_keep_alive(),

    case gen_tcp:send(Socket, Keep_alive) of
        ok ->
            {ok, TimerRef};
        {error, Reason} ->
            erlang:cancel(TimerRef),
            {error, Reason}
    end.

msg_bitfield(Bitfield_length, Bitfield) when is_binary(Bitfield)->
    lager:debug("~p: ~p: length '~p', bitfield '~p'",
                [?MODULE, ?FUNCTION_NAME, Bitfield_length, Bitfield]),
    Length = 1 + Bitfield_length,
    {ok, <<Length:32/big-integer, ?BITFIELD:8/big-integer, Bitfield/big-binary>>}.

msg_request(Index, Begin, Length) ->
    {ok, <<13:32, ?REQUEST:8/big, Index:32, Begin:32, Length:32>>}.

msg_piece(Block_size, Index, Begin, Block) ->
    Length = 9 + Block_size,
    {ok, <<Length:32, ?PIECE:8/big-integer, Index:32, Begin:32, Block:32>>}.

msg_cancel(Index, Begin, Length) ->
    {ok, <<13:32, ?CANCEL:8/big-integer, Index:32, Begin:32, Length:32>>}.

msg_port(Listen_port) ->
    {ok, <<3:32, ?PORT:8/big-integer, Listen_port:32>>}.

send_message(Socket, {Type, Message}) ->
    case gen_tcp:send(Socket, Message) of
        ok ->
            ok;
        {error, Reason} ->
            lager:warning("~p: ~p: failed to send '~p' '~p', reason '~p'",
                          [?MODULE, ?FUNCTION_NAME, Type, Message, Reason]),
            error
    end.

send_bitfield(Socket, Bitfield) ->
    Bitfield_len = bit_size(Bitfield),

    {ok, Bitfield_msg} = msg_bitfield(Bitfield_len, Bitfield),

    send_message(Socket, {bitfield, Bitfield_msg}).

send_handshake(Socket, Peer_id_str, Torrent_info_bin) ->
    % TODO Check if this is correct after peer_id change
    Peer_id_bin = list_to_binary(Peer_id_str),
    {ok, Handshake_msg} = msg_handshake(Torrent_info_bin,
                                                   Peer_id_bin),
    send_message(Socket, {handshake, Handshake_msg}).

send_interested(Socket) ->
    {ok, Interested_msg} = msg_interested(),

    send_message(Socket, {interested, Interested_msg}).

send_unchoke(Socket) ->
    {ok, Unchoke_msg} = msg_unchoke(),

    send_message(Socket, {unchoke, Unchoke_msg}).

-module(ertorrent_peer_proto).

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

-export([
         send_keep_alive/2,
         send_message/2,
         send_bitfield/2,
         send_handshake/3,
         send_interested/1,
         send_unchoke/1
        ]).

-include_lib("eunit/include/eunit.hrl").

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

-define(HANDSHAKE_MSG, <<19:8/integer,
                         "BitTorrent protocol",
                         0:64/integer,
                         Info_hash:160/bitstring,
                         Peer_id:160/bitstring>>).

-define(KEEP_ALIVE_MSG, <<0:32>>).

-define(CHOKE_MSG, <<1:32/integer, ?CHOKE:8/integer>>).
-define(UNCHOKE_MSG, <<1:32/integer, ?UNCHOKE:8/integer>>).
-define(INTERESTED_MSG, <<1:32/integer, ?INTERESTED:8/integer>>).
-define(NOT_INTERESTED_MSG, <<1:32/integer, ?NOT_INTERESTED:8/integer>>).
-define(HAVE_MSG, <<5:32/integer, ?HAVE:8/integer, Piece_index:32/integer>>).
-define(BITFIELD_MSG, <<Length:32/integer, ?BITFIELD:8/integer, Bitfield/bitstring>>).
-define(REQUEST_MSG, <<13:32/integer, ?REQUEST:8/integer, Index:32/integer, Begin:32/integer, Length:32/integer>>).
-define(PIECE_MSG, <<Length:32/integer, ?PIECE:8/integer, Index:32/integer, Begin:32/integer, Block/binary>>).
-define(CANCEL_MSG, <<13:32/integer, ?CANCEL:8/integer, Index:32/integer, Begin:32/integer, Length:32/integer>>).
-define(PORT_MSG, <<3:32/integer, ?PORT:8/integer, Listen_port:32/integer>>).

-record('peer.msg.announce', { info_hash = <<>>, peer_id = <<>> }).
-record('peer.msg.keep_alive', {}).
-record('peer.msg.choke', {}).
-record('peer.msg.unchoke', {}).
-record('peer.msg.interested', {}).
-record('peer.msg.not_interested', {}).
-record('peer.msg.have', { piece_index :: integer() }).
-record('peer.msg.bitfield', { length :: integer(), bitfield = <<>> } ).
-record('peer.msg.request', {
        index :: integer(),
        begins :: integer(),
        length :: integer()
    }).
-record('peer.msg.piece', {
        length :: integer(),
        index :: integer(),
        begins :: integer(),
        block = <<>>
    }).
-record('peer.msg.cancel', {
        index :: integer(),
        begins :: integer(),
        length :: integer()
    }).
-record('peer.msg.port', { port :: integer() }).

msg_handshake(Info_hash, Peer_id) ->
    %Flags = 16#0004000000000000,
    {ok, ?HANDSHAKE_MSG}.

msg_keep_alive() ->
    {ok, ?KEEP_ALIVE_MSG}.

msg_choke() ->
    {ok, ?CHOKE_MSG}.

msg_unchoke() ->
    {ok, ?UNCHOKE_MSG}.

msg_interested() ->
    {ok, ?INTERESTED_MSG}.

msg_not_interested() ->
    {ok, ?NOT_INTERESTED_MSG}.

msg_have(Piece_index) ->
    {ok, ?HAVE_MSG}.

msg_bitfield(Bitfield_length, Bitfield) when is_binary(Bitfield)->
    lager:debug("~p: ~p: length '~p', bitfield '~p'",
                [?MODULE, ?FUNCTION_NAME, Bitfield_length, Bitfield]),
    Length = 1 + Bitfield_length,
    {ok, ?BITFIELD_MSG}.

msg_request(Index, Begin, Length) ->
    {ok, ?REQUEST_MSG}.

msg_piece(Block_size, Index, Begin, Block) ->
    Length = 9 + Block_size,
    {ok, ?PIECE_MSG}.

msg_cancel(Index, Begin, Length) ->
    {ok, ?CANCEL_MSG}.

msg_port(Listen_port) ->
    {ok, ?PORT_MSG}.

send_keep_alive(Socket, Timer) ->
    TimerRef = erlang:send_after(Timer, self(), {keep_alive_internal_timeout}),

    {ok, Keep_alive} = msg_keep_alive(),

    case gen_tcp:send(Socket, Keep_alive) of
        ok ->
            {ok, TimerRef};
        {error, Reason} ->
            erlang:cancel_timer(TimerRef),
            {error, Reason}
    end.

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

parse_msg(?KEEP_ALIVE_MSG) ->
    {ok, #'peer.msg.keep_alive'{}};
parse_msg(?CHOKE_MSG) ->
    {ok, #'peer.msg.choke'{}};
parse_msg(?UNCHOKE_MSG) ->
    {ok, #'peer.msg.unchoke'{}};
parse_msg(?INTERESTED_MSG) ->
    {ok, #'peer.msg.interested'{}};
parse_msg(?NOT_INTERESTED_MSG) ->
    {ok, #'peer.msg.not_interested'{}};
parse_msg(?HAVE_MSG) ->
    {ok, #'peer.msg.have'{ piece_index = Piece_index }};
parse_msg(?REQUEST_MSG) ->
    {ok, #'peer.msg.request'{
        index = Index,
        begins = Begin,
        length = Length
    }};
parse_msg(?PIECE_MSG) ->
    {ok, #'peer.msg.piece'{
        length = Length,
        index = Index,
        begins = Begin,
        block = Block
    }};
parse_msg(?CANCEL_MSG) ->
    {ok, #'peer.msg.cancel'{ index = Index, begins = Begin, length = Length }};
parse_msg(?HANDSHAKE_MSG) ->
    {ok, #'peer.msg.announce'{ info_hash = Info_hash, peer_id = Peer_id }};
parse_msg(?BITFIELD_MSG) ->
    {ok, #'peer.msg.bitfield'{ length = Length, bitfield = Bitfield}};
parse_msg(?PORT_MSG) ->
    {ok, #'peer.msg.port'{ port = Listen_port }}.

-ifdef(EUNIT).
-define(TEST_INFO_HASH, <<"c12fe1c06bba254a9dc9">>).
-define(TEST_PEER_ID, <<"ET001-abcdefghijklmn">>).

handshake_test() ->
    {ok, Msg} = msg_handshake(?TEST_INFO_HASH, ?TEST_PEER_ID),
    {ok, #'peer.msg.announce'{info_hash = Info_hash, peer_id = Peer_id}} = parse_msg(Msg),
    ?assertEqual(?TEST_INFO_HASH, Info_hash),
    ?assertEqual(?TEST_PEER_ID, Peer_id).

keep_alive_test() ->
    {ok, Msg} = msg_keep_alive(),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.keep_alive'{}, Parsed).

choke_test() ->
    {ok, Msg} = msg_choke(),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.choke'{}, Parsed).

unchoke_test() ->
    {ok, Msg} = msg_unchoke(),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.unchoke'{}, Parsed).

interested_test() ->
    {ok, Msg} = msg_interested(),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.interested'{}, Parsed).

not_interested_test() ->
    {ok, Msg} = msg_not_interested(),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.not_interested'{}, Parsed).

have_test() ->
    {ok, Msg} = msg_have(42),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.have'{ piece_index = 42 }, Parsed).

bitfield_test() ->
    Expected = <<1:1, 1:1, 1:1, 0:1, 0:1, 0:1, 0:1, 1:1>>,
    {ok, Msg} = msg_bitfield(binary:referenced_byte_size(Expected), Expected),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.bitfield'{ length = 2, bitfield = Expected }, Parsed).

request_test() ->
    {ok, Msg} = msg_request(42, 4444, 5555),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.request'{
                    index = 42,
                    begins = 4444,
                    length = 5555 }, Parsed).

piece_test() ->
    Expected = <<4:4, 2:4, 0:4, 1:4>>,
    {ok, Msg} = msg_piece(byte_size(Expected), 12, 4096, Expected),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.piece'{
                    length = 11,
                    index = 12,
                    begins = 4096,
                    block = Expected }, Parsed).

cancel_test() ->
    {ok, Msg} = msg_cancel(42, 4444, 5555),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.cancel'{
                    index = 42,
                    begins = 4444,
                    length = 5555 }, Parsed).

port_test() ->
    {ok, Msg} = msg_port(424242),
    {ok, Parsed} = parse_msg(Msg),
    ?assertEqual(#'peer.msg.port'{ port = 424242 }, Parsed).

-endif.

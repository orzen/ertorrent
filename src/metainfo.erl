-module(metainfo).

-export([read_file/1,
         split_pieces/2,
         get_value/2,
         get_info_value/2,
         parse_magnet/1]).

-import(lists, [map/2]).

read_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {ok, {dict, Total}} = bencode:decode(Data),
    {ok, Total}.

split_pieces(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
split_pieces(<<Piece:160/integer, Rest/binary>>, Acc) ->
    [Hex] = io_lib:format("~40.16.0b", [Piece]),
    split_pieces(Rest, [Hex | Acc]).

get_value(Key, Metainfo) ->
    case lists:keyfind(Key, 1, Metainfo) of
        {_, Value} ->
            {ok, Value};
        false ->
            {error, no_match}
    end.

get_info_value(Key, Metainfo) ->
    case lists:keyfind(<<"info">>, 1, Metainfo) of
        {_, Value} ->
            get_value(Key, Value);
        false ->
            {error, no_info_entry}
    end.

parse_magnet(Uri) ->
    Urn = string:substr(Uri, 9),
    Urn_list = string:tokens(Urn, "&"),
    Parsed = parse_urn(Urn_list, []),
    Decoded = decode_magnet(Parsed),
    {ok, Decoded}.

parse_urn([], Acc) ->
    lists:reverse(Acc);
parse_urn([H|T], Acc) ->
    {Key, Value} = parse_key_val(H, []),
    Acc2 = [{Key, Value}|Acc],
    parse_urn(T, Acc2).

parse_key_val([], Acc) ->
    {[], Acc};
parse_key_val([$=|Tail], Acc) ->
    {lists:reverse(Acc), Tail};
parse_key_val([H|Tail], Acc) ->
    parse_key_val(Tail, [H|Acc]).

decode_magnet(Magnet) ->
    decode_magnet(Magnet, []).

decode_magnet([], Acc) ->
    lists:reverse(Acc);
decode_magnet([H|Rest], Acc) ->
    {Key, Value} = H,
    {ok, Decoded_key} = uri:decode(Key),
    {ok, Decoded_value} = uri:decode(Value),
    New_acc = [{Decoded_key, Decoded_value}| Acc],
    decode_magnet(Rest, New_acc).

-module(metainfo).
-include("metainfo.hrl").

-export([parse_file/1, parse_magnet/1]).

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {ok, {dict, Total}} = bencode:decode(Data),
    {ok, parse_file(Total, #metainfo{})}.

parse_file([], Record) ->
    Record;
parse_file([{<<"announce">>, Value}|Tail], Record) ->
    New_record = Record#metainfo{announce=Value},
    parse_file(Tail, New_record);
parse_file([{<<"announce-list">>, Value}|Tail], Record) ->
    New_record = Record#metainfo{announce_list=Value},
    parse_file(Tail, New_record);
parse_file([{<<"comment">>, Value}|Tail], Record) ->
    New_record = Record#metainfo{comment=Value},
    parse_file(Tail, New_record);
parse_file([{<<"creation date">>, Value}|Tail], Record) ->
    New_record = Record#metainfo{creation_date=Value},
    parse_file(Tail, New_record);
parse_file([{<<"httpseeds">>, Value}|Tail], Record) ->
    New_record = Record#metainfo{httpseeds=Value},
    parse_file(Tail, New_record);
parse_file([{<<"info">>, Value}|Tail], Record) ->
    {dict, Info_data} = Value,
    {ok, Info_bencoded} = bencode:encode(Value),
    Info_record = parse_info(Info_data, #info{}),
    {ok, Info_encoded} = utils:encode_hash(Info_bencoded),
    New_record = Record#metainfo{info_hash=Info_encoded, info=Info_record},
    parse_file(Tail, New_record).

parse_info([], Record) ->
    Record;
parse_info([{<<"length">>, Value}|Tail], Record) ->
    New_record = Record#info{length=Value},
    parse_info(Tail, New_record);
parse_info([{<<"name">>, Value}|Tail], Record) ->
    New_record = Record#info{name=Value},
    parse_info(Tail, New_record);
parse_info([{<<"piece length">>, Value}|Tail], Record) ->
    New_record = Record#info{piece_length=Value},
    parse_info(Tail, New_record);
parse_info([{<<"pieces">>, Value}|Tail], Record) ->
    New_record = Record#info{pieces=Value},
    parse_info(Tail, New_record).

parse_magnet(Uri) ->
    case utils:is_magnet(Uri) of
        true ->
            Urn = string:substr(Uri, 9),
            Urn_list = string:tokens(Urn, "&"),
            Parsed = parse_urn(Urn_list, []),
            Decoded = decode_magnet(Parsed),
            {ok, Decoded};
        false ->
            {error, "Invalid magnet"}
    end.

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

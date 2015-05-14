-module(magnet_uri).

-export([is_magnet/1, parse/1]).

is_magnet(Str) ->
    case string:str(Str, "magnet:?") == 1 of
        true -> true;
        false -> false
    end.

parse(Uri) ->
    case is_magnet(Uri) of
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

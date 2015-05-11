-module(magnet_uri).

-export([decode/1]).

decode(Uri) ->
    case string:str(Uri, "magnet:?") == 1 of
        true ->
            Urn = string:substr(Uri, 9),
            Urn_list = string:tokens(Urn, "&"),
            {ok, parse_urn(Urn_list, [])};
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

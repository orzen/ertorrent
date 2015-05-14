-module(uri).

-export([decode/1]).

decode(Str) ->
    {ok, decode(Str, [])}.

decode([], Acc) ->
    lists:reverse(Acc);
decode([$%, Msn, Lsn|Rest], Acc) ->
    Res = hexstr_to_dec([Msn, Lsn]),
    decode(Rest, [Res|Acc]);
decode([H|Rest], Acc) ->
    decode(Rest, [H|Acc]).

hexstr_to_dec(Hexstr) ->
    List = lists:reverse(Hexstr),
    hexstr_to_dec(List, 0, 0).

hexstr_to_dec([], _, Acc) ->
    trunc(Acc);
hexstr_to_dec([H|Rest], Counter, Acc) ->
    Dec = hex_to_dec(H),
    New_acc = Dec * math:pow(16, Counter) + Acc,
    hexstr_to_dec(Rest, (Counter + 1), New_acc).

hex_to_dec(X) when (X >= $0) andalso (X =< $9) -> (X - $0);
hex_to_dec(X) when (X >= $A) andalso (X =< $F) -> (X - $A + 10);
hex_to_dec(X) when (X >= $a) andalso (X =< $f) -> (X - $a + 10).

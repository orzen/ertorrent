-module(uri).

-export([decode/1, encode/1]).

decode(Str) ->
    {ok, decode(Str, [])}.

encode(Str) ->
    Reserved_list = sets:from_list([$ , $:, $/, $?, $#, $[, $], $@, $!, $$, $&,
                                    $', $(, $), $*, $, , $;, $=]),
    {ok, dec_to_hex(Str, Reserved_list, [])}.

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

dec_to_hex([], _, Acc) ->
    lists:reverse(Acc);
dec_to_hex([H|Tail], Reserved_list, Acc) ->
    case sets:is_element(H, Reserved_list) of
        true ->
            [Msn, Lsn] = integer_to_list(H, 16),
            New_acc = [Lsn, Msn, $%|Acc],
            dec_to_hex(Tail, Reserved_list, New_acc);
        false ->
            New_acc = [H|Acc],
            dec_to_hex(Tail, Reserved_list, New_acc)
    end.

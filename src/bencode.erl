-module(bencode).

-export([decode/1, encode/1]).

decode(File) ->
    {ok, Data} = file:read_file(File),
    {ok, dec(Data)}.

encode(Struct) ->
    {ok, iolist_to_binary(enc(Struct))}.

dec(<<$d, Tail/binary>>) ->
    dec_dict(Tail, []);
dec(<<$l, Tail/binary>>) ->
    dec_list(Tail, []);
dec(<<$i, Tail/binary>>) ->
    dec_int(Tail, []);
dec(Data) ->
    dec_string(Data, []).

dec_int(<<$e, Tail/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Tail};
dec_int(<<X, Tail/binary>>, Acc) ->
    dec_int(Tail, [X|Acc]).

dec_string(<<$:, Tail/binary>>, Acc)->
    Int = list_to_integer(lists:reverse(Acc)),
    <<Str:Int/binary, Rest/binary>> = Tail,
    {Str, Rest};
dec_string(<<X, Tail/binary>>, Acc) ->
    dec_string(Tail, [X|Acc]).

dec_list(<<$e, Tail/binary>>, Acc) ->
    {{list, lists:reverse(Acc)}, Tail};
dec_list(Data, Acc) ->
    {Res, Tail} = dec(Data),
    dec_list(Tail, [Res|Acc]).

dec_dict(<<$e, Tail/binary>>, Acc) ->
    {{dict, lists:reverse(Acc)}, Tail};
dec_dict(Data, Acc) ->
    {Key, Tail1} = dec(Data),
    {Value, Tail2} = dec(Tail1),
    H = {Key,Value},
    dec_dict(Tail2, [H|Acc]).


enc(Int) when is_integer(Int) ->
    IntBin = list_to_binary(integer_to_list(Int)),
    [$i, IntBin, $e];
enc(Str) when is_list(Str) ->
    enc(list_to_binary(Str));
enc(Str) when is_binary(Str) ->
    IntBin = list_to_binary(integer_to_list(size(Str))),
    [IntBin, $:, Str];
enc(Tuple) when is_tuple(Tuple), (element(1, Tuple) /= dict), (element(1, Tuple) /= list) ->
    {Key, Value} = Tuple,
    [enc(Key), enc(Value)];
enc({list, List}) when is_list(List) ->
    [$l, [enc(Elem) || Elem <- List], $e];
enc({dict, Dict}) when is_list(Dict)->
    [$d, [enc(Elem) || Elem <- Dict], $e].

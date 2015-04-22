-module(bencode).

-export([decode/1]).

decode(File) ->
    {ok, Data} = file:read_file(File),
    dec(Data).

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
    {{dict, Acc}, Tail};
dec_dict(Data, Acc) ->
    {Key, Tail1} = dec(Data),
    {Value, Tail2} = dec(Tail1),
    H = {Key,Value},
    dec_dict(Tail2, [H|Acc]).

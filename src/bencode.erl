-module(bencode).

-export([decode/1, encode/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

decode(Data) ->
    Binary_data = iolist_to_binary(Data),
    case dec(Binary_data) of
        {{dict, Value}, _} -> Return = {dict, Value};
        {{list, Value}, _} -> Return = {list, Value};
        {Value, _} -> Return = Value
    end,
    {ok, Return}.

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
    H = {Key, Value},
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

% Tests
-ifdef(TEST).

%Test decoding list nested inside a dict
decode_nested_1_test() ->
    erlang:display("Test decode nested 1"),
    {ok, Dict} = decode(<<"d3:key5:value3:fool3:bar3:bazee">>),
    ?assert(Dict =:= {dict,
                      [{<<"key">>, <<"value">>},
                       {<<"foo">>, {list, [<<"bar">>, <<"baz">>]}}]}).

%Test decoding dict nested inside a list
decode_nested_2_test() ->
    erlang:display("Test decode nested 2"),
    {ok, List} = decode(<<"ld3:foo3:baree">>),
    ?assert(List =:= {list, [{dict, [{<<"foo">>, <<"bar">>}]}]}).

%Test decoding plain dict
decode_dict_test() ->
    erlang:display("Test decode dict"),
    {ok, Dict} = decode(<<"d3:key5:value3:foo3:bare">>),
    ?assert(Dict =:= {dict, [{<<"key">>, <<"value">>}, {<<"foo">>, <<"bar">>}]}).

%Test decoding plain list
decode_list_test() ->
    erlang:display("Test decode list"),
    {ok, List} = decode(<<"l3:foo3:bare">>),
    ?assert(List =:= {list, [<<"foo">>, <<"bar">>]}).

%Test decoding plain integer
decode_int_test() ->
    erlang:display("Test decode integer"),
    {ok, Integer} = decode(<<"i42e">>),
    ?assert(Integer =:= 42).

%Test decoding plain string
decode_string_test() ->
    erlang:display("Test decode string"),
    {ok, String} = decode(<<"3:foo">>),
    ?assert(String =:= <<"foo">>).

%Test encoding a list nested in a dict
encode_nested_1_test() ->
    erlang:display("Test encode nested 1"),
    Dict = {dict,
            [{<<"key">>, <<"value">>},
             {<<"foo">>, {list, [<<"bar">>, <<"baz">>]}}]},
    {ok, String} = encode(Dict),
    ?assert(String =:= <<"d3:key5:value3:fool3:bar3:bazee">>).

%Test encoding a dict nested in a list
encode_nested_2_test() ->
    erlang:display("Test encode nested 2"),
    List = {list, [{dict, [{<<"foo">>, <<"bar">>}]}]},
    {ok, String} = encode(List),
    ?assert(String =:= <<"ld3:foo3:baree">>).

%Test encoding plain dict
encode_dict_test() ->
    erlang:display("Test encode dict"),
    Dict = {dict, [{<<"key">>, <<"value">>}, {<<"foo">>, <<"bar">>}]},
    {ok, String} = encode(Dict),
    ?assert(String =:= <<"d3:key5:value3:foo3:bare">>).

%Test encoding plain list
encode_list_test() ->
    erlang:display("Test encode list"),
    List = {list, [<<"foo">>, <<"bar">>]},
    {ok, String} = encode(List),
    ?assert(String =:= <<"l3:foo3:bare">>).

%Test encoding plain integer
encode_int_test() ->
    erlang:display("Test encode integer"),
    Integer = 42,
    {ok, String} = encode(Integer),
    ?assert(String =:= <<"i42e">>).

%Test encoding plain string
encode_string_test() ->
    erlang:display("Test encode string"),
    Str = "foobar",
    {ok, String} = encode(Str),
    ?assert(String =:= <<"6:foobar">>).

-endif.

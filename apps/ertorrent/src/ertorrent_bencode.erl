-module(ertorrent_bencode).

-export([decode/1,
         encode/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

decode(Data) ->
    [Decoded] = decode(iolist_to_binary(Data), []),
    {ok, Decoded}.

%% Done parsing
decode(<<"">>, Acc) ->
    lists:reverse(Acc);

%% Reached the end of a container type
decode(<<$e, Tail/binary>>, Acc) ->
    {Tail, lists:reverse(Acc)};

%% Parsing integer
decode(<<$i, Tail/binary>>, Acc) ->
    {Int, New_tail} = parse_int(Tail, 0),
    decode(New_tail, [Int|Acc]);

%% Parsing list
decode(<<$l, Tail/binary>>, Acc) ->
    {New_tail, List} = decode(Tail, []),
    decode(New_tail, [List|Acc]);

%% Parsing dict
decode(<<$d, Tail/binary>>, Acc) ->
    % Treat the dict as a list and then convert it properly
    {New_tail, List} = decode(Tail, []),
    {ok, Dict} = list_to_dict(List, []),
    decode(New_tail, [Dict|Acc]);

%% Parsing string
decode(<<Any/binary>>, Acc)  ->
    {Length, Int_tail} = parse_int(Any, 0),
    <<String:Length/binary, String_tail/binary>> = Int_tail,
    decode(String_tail, [String|Acc]).

encode(Term) ->
    {ok, encode_term(Term)}.

%% Encode integer
encode_term(H) when is_integer(H) ->
    Binary_integer = integer_to_binary(H),
    list_to_binary([$i, Binary_integer, $e]);

%% Encode string
encode_term(H) when is_bitstring(H) ->
    Length = byte_size(H),
    Binary_length = integer_to_binary(Length),
    list_to_binary([Binary_length, $:, H]);

%% Determine the container type
encode_term(H) when is_list(H) ->
    case is_dict(H) of
        true ->
            encode_dict(H, []);
        false ->
            encode_list(H, [])
    end.

%% Encode dict
encode_dict([], Acc) ->
    Reverse_dict = lists:reverse([<<$e>>|Acc]),
    list_to_binary([<<$d>>|Reverse_dict]);
encode_dict([{Key, Value}|Tail], Acc) ->
    Encoded_key = encode_term(Key),
    Encoded_value = encode_term(Value),
    encode_dict(Tail, [Encoded_value, Encoded_key|Acc]).

%% Encode list
encode_list([], Acc) ->
    Reverse_list = lists:reverse([<<$e>>|Acc]),
    list_to_binary([<<$l>>|Reverse_list]);
encode_list([H|Tail], Acc) ->
    Encoded_value = encode_term(H),
    encode_list(Tail, [Encoded_value|Acc]).

%%% UTILITY FUNCTIONS

%% Validate if term is dict()
%% dict() -> [{key1, value1}, {key2, value2}}]
is_dict([]) ->
    true;
is_dict([H|Tail]) ->
    case is_tuple(H)
         andalso tuple_size(H) =:= 2
         andalso is_bitstring(element(1, H)) of
        true -> is_dict(Tail);
        false -> false
    end.

%% Parsing both integer value and string prefix
parse_int(<<$e, Tail/binary>>, Acc) ->
    {Acc, Tail};
parse_int(<<$:, Tail/binary>>, Acc) ->
    {Acc, Tail};
parse_int(<<Ascii_nbr/integer, Tail/binary>>, Acc) ->
    Integer_part = Ascii_nbr-$0,
    case Integer_part >= 0 andalso Integer_part =< 9 of
        true ->
            parse_int(Tail, Acc * 10  + Integer_part);
        false ->
            {error, "failed to parse"}
    end.

%% Convert list to dict()
%% dict() -> [{key1, value1}, {key2, value2}}]
list_to_dict([], Acc) ->
    {ok, lists:reverse(Acc)};
list_to_dict([Key, Value|Tail], Acc) ->
    list_to_dict(Tail, [{Key, Value}|Acc]).

%%% Tests
-ifdef(TEST).

% Test decoding list nested inside a dict
decode_nested_1_test() ->
    {ok, Dict} = decode(<<"d3:key5:value3:fool3:bar3:bazee">>),
    ?assert(Dict =:= [{<<"key">>, <<"value">>}, {<<"foo">>, [<<"bar">>,
                                                             <<"baz">>]}]).

% Test decoding dict nested inside a list
decode_nested_2_test() ->
    {ok, List} = decode(<<"ld3:foo3:baree">>),
    ?assert(List =:= [[{<<"foo">>, <<"bar">>}]]).

% Test decoding plain dict
decode_dict_test() ->
    {ok, Dict} = decode(<<"d3:key5:value3:foo3:bare">>),
    ?assert(Dict =:= [{<<"key">>, <<"value">>}, {<<"foo">>, <<"bar">>}]).

% Test decoding plain list
decode_list_test() ->
    {ok, List} = decode(<<"l3:foo3:bare">>),
    ?assert(List =:= [<<"foo">>, <<"bar">>]).

% Test decoding plain integer
decode_int_test() ->
    {ok, Integer} = decode(<<"i42e">>),
    ?assert(Integer =:= 42).

% Test decoding plain string
decode_string_test() ->
    {ok, String} = decode(<<"3:foo">>),
    ?assert(String =:= <<"foo">>).

% Test encoding a list nested in a dict
encode_nested_1_test() ->
    Dict = [{<<"key">>, <<"value">>}, {<<"foo">>, [<<"bar">>, <<"baz">>]}],
    {ok, String} = encode(Dict),
    ?assert(String =:= <<"d3:key5:value3:fool3:bar3:bazee">>).

% Test encoding a dict nested in a list
encode_nested_2_test() ->
    List = [[{<<"foo">>, <<"bar">>}]],
    {ok, String} = encode(List),
    ?assert(String =:= <<"ld3:foo3:baree">>).

% Test encoding plain dict
encode_dict_test() ->
    Dict = [{<<"key">>, <<"value">>}, {<<"foo">>, <<"bar">>}],
    {ok, String} = encode(Dict),
    ?assert(String =:= <<"d3:key5:value3:foo3:bare">>).

% Test encoding plain list
encode_list_test() ->
    List = [<<"foo">>, <<"bar">>],
    {ok, String} = encode(List),
    ?assert(String =:= <<"l3:foo3:bare">>).

% Test encoding plain integer
encode_int_test() ->
    Integer = 42,
    {ok, String} = encode(Integer),
    ?assert(String =:= <<"i42e">>).

% Test encoding plain string
encode_string_test() ->
    {ok, String} = encode(<<"foobar">>),
    ?assert(String =:= <<"6:foobar">>).

-endif.

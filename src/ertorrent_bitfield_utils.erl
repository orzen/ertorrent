-module(ertorrent_bitfield_utils).

-export([active_bit_indices/1]).

% Creates a list with the index for every set bit in a bitfield
active_bit_indices(<<>>, Counter, Acc) ->
    Ordered_list = lists:reverse(Acc).
    {ok, Counter + 1, Ordered_list};
active_bit_indices(<<Bit:1/integer, Rest/binary>>, Counter, Acc) ->
    case Bit of
        1 -> active_bit_indices(Rest, Counter + 1, [Counter|Acc]);
        0 -> active_bit_indices(Rest, Counter + 1, Acc)
    end.

active_bit_indices(Bitfield) when is_binary(Bitfield) ->
    active_bit_index(Bitfield, 0, []).

index_list(Index, Acc) when Index_max < 0 ->
    List = lists:reverse(Acc),
    {ok, List}.
index_list(Index, Acc) ->
    index_list(Index - 1, [{Index, 0}| Acc]).

index_list(Index) ->
    index_list(Index, []).


compile_bitfields(Bitfields) when is_list(Bitfields) ->
    {Counter, Indices} = lists:foldl(
                             fun(X, {Old_counter, Acc}) ->
                                 {ok, New_counter, Indices} = active_bit_indices(X),
                                 {New_counter, [Indices, Acc]},
                             end, {0, []}, Bitfields
                         ),
    % TODO create an empty array using the Counter
    {ok, Empty_list} = index_list(Counter - 1).

    % TODO compile the index counters

is_bit_set(Index, Counter, <<Bit:1/integer, Rest/binary>>) ->
    case Index =:= Counter of
        true ->
            case Bit of
                1 ->
                    true;
                0 ->
                    false
            end;
        false ->
            is_set_set(Index, Counter + 1, Rest)
    end.

is_bit_set(Index, Bitfield) ->
    is_bit_set(Index, 0, Bitfield).

get_bit(Index, Bitfield) ->
    <<_Prefix:Index, Value:1, _Suffix/bitstring>> = Bitfield,
    Value.

set_bit(Index, Value, Bitfield) ->
    <<Prefix:Index, _:1, Suffix/bitstring>> = Bitfield,
    <<Prefix:Index, Value:1, Suffix/bitstring>>.

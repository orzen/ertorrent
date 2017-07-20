-module(ertorrent_bitfield_utils).

-export([get_bit/2,
         set_bit/3,
         list_to_bitfield/1,
         bin_reverse/1]).

%% Get bit at index
get_bit(Index, Bitfield) when is_bitstring(Bitfield)->
    <<_Prefix:Index/bitstring, Bit:1/integer, _Rest/bitstring>> = Bitfield,
    {ok, Bit}.

%% Set bit at index
set_bit(Index, Value, Bitfield) ->
    <<Prefix:Index/bitstring, _:1, Suffix/bitstring>> = Bitfield,
    <<Prefix:Index/bitstring, Value:1/bitstring, Suffix/bitstring>>.

% Converting a list consisting of 0 and 1 to a bitstring
list_to_bitfield(List) when is_list(List) ->
    % Convert the list into a binary as a base for the bitstring comprehension
    Bin = list_to_binary(List),

    % Contructing a new binary with each of the byte values from Bin as bits
    Bitfield = << <<X:1>> || <<X>> <= Bin >>,

    {ok, Bitfield}.

% Reversing the binary order as reversing a list
bin_reverse(Bin) when is_binary(Bin) ->
    Reversed_bin = bin_reverse1(Bin, <<>>),
    {ok, Reversed_bin}.

bin_reverse1(<<>>, Acc) ->
    Acc;
bin_reverse1(<<Bit:1/binary, Rest/binary>>, Acc) ->
    bin_reverse1(Rest, <<Bit:1/binary, Acc/binary>>).

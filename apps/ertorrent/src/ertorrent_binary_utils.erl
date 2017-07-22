-module(ertorrent_binary_utils).

-export([bin_reverse/1,
         bitfield_to_list/1,
         get_bit/2,
         list_to_bitfield/1,
         set_bit/3,
         sum_bitfields/1,
         parse_peers/1]).

% Reversing the binary order as reversing a list
bin_reverse(Bin) when is_binary(Bin) ->
    Reversed_bin = bin_reverse1(Bin, <<>>),
    {ok, Reversed_bin}.

bin_reverse1(<<>>, Acc) ->
    Acc;
bin_reverse1(<<Bit:1/binary, Rest/binary>>, Acc) ->
    bin_reverse1(Rest, <<Bit:1/binary, Acc/binary>>).

bitfield_to_list(Bitfield) when is_bitstring(Bitfield)->
    Bitfield_list = bitfield_to_list1(Bitfield, []),
    {ok, Bitfield_list}.

bitfield_to_list1(<<>>, Acc)->
    lists:reverse(Acc);
bitfield_to_list1(<<Bit:1/integer, Rest/bitstring>>, Acc) ->
    bitfield_to_list1(Rest, [Bit| Acc]).

%% Get bit at index
get_bit(Index, Bitfield) when is_bitstring(Bitfield)->
    <<_Prefix:Index/bitstring, Bit:1/integer, _Rest/bitstring>> = Bitfield,
    {ok, Bit}.

% Converting a list consisting of 0 and 1 to a bitstring
list_to_bitfield(List) when is_list(List) ->
    % Convert the list into a binary as a base for the bitstring comprehension
    Bin = list_to_binary(List),

    % Contructing a new binary with each of the byte values from Bin as bits
    Bitfield = << <<X:1>> || <<X>> <= Bin >>,

    {ok, Bitfield}.

% Parsing the binary representation of the peers-section from a tracker response
parse_peers(Peers) when is_binary(Peers) ->
    Peers_list = parse_peers1(Peers, []),
    {ok, Peers_list}.

parse_peers1(<<>>, Acc) ->
    Acc;
parse_peers1(<<Address_int:4/integer, Port:2/integer, Rest/binary>>, Acc) ->
    % Format address from string to ip_address(), a tuple based quad format
    {ok, Address} = inet:parse_ipv4_address(Address_int),
    parse_peers1(Rest, [{Address, Port}| Acc]).

%% Set bit at index
set_bit(Index, Value, Bitfield) ->
    <<Prefix:Index/bitstring, _:1, Suffix/bitstring>> = Bitfield,
    <<Prefix:Index/bitstring, Value:1/bitstring, Suffix/bitstring>>.

sum_bitfields(Bitfields) when is_list(Bitfields)->
    % Convert the bitfield from a binary to a list to be able to use the list functions
    Bitfield_lists = lists:foldl(fun(Bitfield_bin, Acc) ->
                                     {ok, Bitfield_list} = bitfield_to_list(Bitfield_bin),

                                     [Bitfield_list| Acc]
                                 end, [], Bitfields),

    % Since sum_bitfields1/2 will use zipwith/3, the head will be used to zip against.
    [H| Rest] = Bitfield_lists,

    Bitfields_sum = sum_bitfields1(Rest, H),

    {ok, Bitfields_sum}.

sum_bitfields1([], Acc) ->
    Acc;
sum_bitfields1([H| Rest], Acc) ->
    New_acc = lists:zipwith(fun(X,Y) ->
                                X + Y
                            end, H, Acc),

    sum_bitfields1(Rest, New_acc).

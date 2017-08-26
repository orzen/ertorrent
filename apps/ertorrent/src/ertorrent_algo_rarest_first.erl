-module(ertorrent_algo_rarest_first).

-export([construct_piece_order/1]).

-define(BINARY, ertorrent_binary_utils).
-define(UTILS, ertorrent_utils).

construct_piece_order(Peer_bitfields) when is_list(Peer_bitfields) ->
    % Filter peer's id from the tuple list Peer_bitfields
    Bitfields_alone = [Bitfield || {_Peers_id, Bitfield}=_L <- Peer_bitfields],

    % Compile all the bitfields into one list to visualize the rarest pieces
    Bitfields_sum = ?BINARY:sum_bitfields(Bitfields_alone),

    % Add indices to the bitfield summary to keep track of the sum for each
    % index before sorting.
    Sum_with_idx = ?UTILS:index_list(Bitfields_sum),

    % Sorting the summary
    Rarest_first = lists:keysort(2, Sum_with_idx),

    {ok, Rarest_first}.

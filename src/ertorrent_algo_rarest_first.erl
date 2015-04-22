-module(ertorrent_algo_rarest_first).

-export([
         rx_next/5,
         rx_init/1,
         rx_update/2
        ]).

-define(BINARY, ertorrent_binary_utils).
-define(UTILS, ertorrent_utils).

% @doc Creating a tuple list of pieces that is missing based on the torrent
% bitfield. This function is used before we receive the peer's bitfield.
% output: [{Index0, Distributed0}, {Index1, Distributed0}]
% @end
-spec rx_init(Torrent_bitfield::list()) -> list().
rx_init(Torrent_bitfield) when is_list(Torrent_bitfield) ->
    % Add index number to each bit in the bitfield e.g. [{Index, Bit}]
    {ok, Bits_with_index} = ?UTILS:index_list(Torrent_bitfield),

    Rx_pieces = lists:filtermap(fun({Index, Bit}) ->
                                    case Bit of
                                        % None of the pieces should have been
                                        % distributed yet since this is the
                                        % initial list. Therefore are
                                        % occurrences set to 0 for all pieces.
                                        0 -> {true, {Index, 0}};
                                        1 -> false
                                    end
                                end, Bits_with_index),

    {ok, Rx_pieces}.


% Returns a sorted tuple list with the rarest piece being the head of the list.
% E.g. [{Piece_index, Piece_occurrences}]
% TODO See if there is a type for tuple list then replace the return in spec
% with tuple list.
-spec rx_update(Own_bitfield::list(), Peer_bitfield::list()) -> list().
rx_update(Own_bitfield, Peer_bitfields) when
      is_list(Own_bitfield) andalso
      length(Own_bitfield) > 0 andalso
      is_list(Peer_bitfields) andalso
      length(Peer_bitfields) > 0 ->
    lager:debug("~p: ~p: own bitfield '~p'~npeer bitfields '~p'",
                [?MODULE, ?FUNCTION_NAME, Own_bitfield, Peer_bitfields]),

    % Compile all the bitfields into one list to visualize the rarest pieces
    {ok, Bitfields_sum} = ?BINARY:sum_bitfields(Peer_bitfields),

    % Add indices to the bitfield summary to keep track of the sum for each
    % index before sorting.
    {ok, Sum_with_idx} = ?UTILS:index_list(Bitfields_sum),

    % Sorting the summary
    Rarest_first = lists:keysort(2, Sum_with_idx),

    % Zip with the own bitfield
    Zipped = lists:zipwith(fun({Index, Occurrences}, Bit) ->
                               {Index, Occurrences, Bit}
                           end, Rarest_first, Own_bitfield),

    % Filter the piece that has already been downloaded
    Filtered = lists:filtermap(fun({Index, Occurrences, Bit}) ->
                                   case Bit of
                                       % Keep the original tuple if the piece is missing
                                       0 -> {true, {Index, Occurrences}};
                                       % Discard the tuple if the piece already been downloaded
                                       1 -> false
                                   end
                               end, Zipped),

    {ok, Filtered}.

-spec rx_next(Peer_bitfield::list(),
              Prioritized_pieces::list(),
              Distributed_pieces::list(),
              Piece_distribution_limit::integer(),
              Piece_queue_limit::integer()) -> list().
rx_next(Peer_bitfield,
        Prioritized_pieces,
        Distributed_pieces,
        Piece_distribution_limit,
        Piece_queue_limit) when
      is_list(Prioritized_pieces) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),

    % Compile a list with pieces that are currently distributed
    Limited_pieces = [Piece_index || {Piece_index, Distributed} <-
                                     Distributed_pieces, Distributed <
                                     Piece_distribution_limit],

    % Filter out the pieces that are already assigned to other peer workers
    WO_limited_pieces = lists:filter(fun({Piece_index, _Occurrences}) ->
                                         not lists:member(Piece_index, Limited_pieces)
                                     end, Prioritized_pieces),

    % If the peer chose to not announce its bitfield, fallback to a stupid
    % selection of trail and error.
    case Peer_bitfield == undefined of
        true ->
            Available_pieces = WO_limited_pieces;
        false ->
            % Find the remaining pieces which the peer possess
            Available_pieces = lists:filtermap(fun({Index, _Occurrences}) ->
                                                   % Check if the peer possess
                                                   % the most relevant piece
                                                   case lists:nth(Index,
                                                                  Peer_bitfield) of
                                                       1 -> true;
                                                       0 -> false
                                                   end
                                               end, WO_limited_pieces)
    end,

    % Apply the queue limit
    Pieces_queue_limit = lists:sublist(Available_pieces, Piece_queue_limit),

    % Remove Occurrences from the response
    Pieces = [Piece_index || {Piece_index, _Occurrences} <- Pieces_queue_limit],

    % Update the list of Distributed_pieces with Pieces
    % Start by filter out the tuples for the outdated pieces
    Map = fun({Distrib_index, Distrib_times}, Acc) ->
              case lists:member(Distrib_index, Acc) of
                  true ->
                      New_acc = lists:delete(Distrib_index, Acc),
                      {{Distrib_index, Distrib_times + 1}, New_acc};
                  false ->
                      {{Distrib_index, Distrib_times}, Acc}
              end
          end,
    % Increase the counter of the existing indices and leave the missing
    % indices in the accumulator.
    {Distrib_pieces_tmp, Missing_indices} = lists:mapfoldl(Map, Pieces, Distributed_pieces),

    % Create a tuple list with the missing indices and the occurrence.
    Formated_indices = [{X, 1} || X <- Missing_indices],

    % Concatenate the tuple list with the existing and the new piece indices.
    New_distrib_pieces = Distrib_pieces_tmp ++ Formated_indices,

    Distrib_pieces_sorted = lists:keysort(1, New_distrib_pieces),

    {ok, Pieces, Distrib_pieces_sorted}.

-module(ertorrent_utils).

-export([index_list/1,
         read_term_from_file/1,
         write_term_to_file/2,
         encode_hash/1,
         pieces_binary_to_list/1]).

%ensure_file_entries({files, multiple, Name, Files}, Location) ->
%    Dir = Location ++ '/' ++ Name,
%
%    % Make sure that the torrent folder exists
%    case ensure_dir(Dir) of
%        ok ->
%            % Create files
%            foreach(fun(X) ->
%                        {ok, Fd} = file:open(Dir ++ '/' ++ X, [write]),
%                        % TODO add support for allocate
%                        % file:allocate(Fd, Offset, Length)
%                        file:close(Fd)
%                    end, Files)
%    end;
%ensure_file_entries({files, single, Name, [File]}, Location) ->
%    {ok, Fd} = file:open(Dir ++ '/' ++ Name),
%    file:close(Fd).


create_file_mapping__unify_file_list(File_paths) ->
    % Unify the tuple list of files and file sizes
    lists:foldl(fun(FileTuple, Acc) ->
                    case FileTuple of
                        {File, Length} ->
                            [{File, Length}| Acc];
                        {File, Length, _} ->
                            [{File, Length}| Acc];
                    end
                end, [], File_paths).

% The peer wire protocol is referring to piece index. This is the total size of
% the download media divided by the piece size. However when the download media
% contains multiple files then the files will be concatenated in the order
% used in the metainfo and then divided by the piece size. File operation are,
% surprisingly, unaware of piece index and instead requires a file path, an
% offset and a byte size. This function will create a list with the mapping
% between piece index and file information.
create_file_mapping(File_paths, Piece_size) ->
    Unified = create_file_mapping__unify_file_list(File_paths),

    Mapping = create_file_mapping1(Unified, Piece_size, 0, []),
    {ok, Mapping}.

create_file_mapping1([], Piece_size, Current_index, Acc) ->
    Acc;
create_file_mapping1([{File, Length}| Rest_files], Piece_size, Current_index, Acc) ->
    % Calculate the amount of whole pieces
    Pieces_num = Length div Piece_size,
    % Calculate the amount of remaining bytes in the last piece
    Piece_rem = Length rem Piece_size,

    % Calulate the index which the next recursion should start from
    Next_index = Current_index + Piece_num,

    % Subtract 1 from the next index to stay within the range
    Index_seq = lists:seq(Current_index, Next_index - 1),

    % Creating a list of tuples with the values:
    % {Piece_index, File_path, Offset_in_bytes, Length_in_bytes}
    File_mapping = [{Index, File, Index * Piece_size, Piece_size} ||
                    Index <- Index_seq],

    case Piece_rem =:= 0 of
        false ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 [{Next_index, File, Index * Piece_size, Piece_rem}],
                                 Acc ++ File_mapping);
        true ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 Acc ++ File_mapping)
    end.

create_file_mapping1([], Piece_size, Current_index, Piece_rem, Acc) ->
    % NOTE Piece_rem already contains the current index
    Acc ++ Piece_rem;
create_file_mapping1([{File, Length}| Rest_files], Piece_size, Current_index, Remainder, Acc) ->
    % Get the size from the previous piece to calulate the remaining
    % portion of that piece from this file.
    [{_, _, _, Previous_size}] = Remainder,
    Previous_piece_rem = Piece_size - Previous_size,

    % Since a part of the beginning of this file will be a part of the previous
    % piece, it will have to be removed from the total length of this file, to
    % know the divisible size and its remainder.
    Remaining_file_size = Length - Previous_piece_rem,

    Piece_num = Remaining_file_size div Piece_size,
    Piece_rem = Remaining_file_size rem Piece_size,

    % Calulate the index which the next recursion should start from
    Next_index = Current_index + Piece_num,

    % Subtract 1 from the next index to stay within the range
    Index_seq = lists:seq(Current_index, Next_index - 1),

    % Creating a list of tuples with the values:
    % {Piece_index, File_path, Offset_in_bytes, Length_in_bytes}
    File_mapping = [{Index, File, Index * Piece_size, Piece_size} ||
                    Index <- Index_seq],

    case Piece_rem =:= 0 of
        false ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 [{Next_index, File, Index * Piece_size, Piece_rem}],
                                 Acc ++ File_mapping);
        true ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 Acc ++ File_mapping)
    end.


%% Creating a tupled list where the first element of the tuple is the index
%% value and the second value is a value of the input list.
index_list(List) when is_list(List) ->
    New_list = index_list1(List, 0, []),
    {ok, New_list}.

index_list1([], _Counter, Acc) ->
    lists:reverse(Acc);
index_list1([H| Rest], Counter, Acc) ->
    index_list1(Rest, (Counter + 1), [{Counter, H}| Acc]).

read_term_from_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    {ok, binary_to_term(Data)}.

write_term_to_file(Filename, Data) ->
    file:write_file(Filename, term_to_binary(Data)).

format_hash([], Acc) ->
    lists:reverse(Acc);
format_hash([Msn, Lsn|Tail], Acc) ->
    New_acc = [Lsn, Msn, $%|Acc],
    format_hash(Tail, New_acc).

encode_hash(Info_bencoded) ->
    % 160bits/8=20 byte SHA1 as integerlist
    <<Hash:160/integer>> = crypto:hash(sha, Info_bencoded),
    % Convert the integerlist to a string with len:40, base:16, type:binary
    Info_hash = lists:flatten(io_lib:format("~40.16.0b", [Hash])),
    % Percent-formatting
    Percent_format = format_hash(Info_hash, []),
    % Upper case
    Upper_format = string:to_upper(Percent_format),
    {ok, Upper_format}.

% Splitting the pieces section from the metainfo into 20-byte segments and
% placing them into a list.
pieces_binary_to_list(Binary_pieces) when is_bitstring(Binary_pieces) ->
    Pieces = pieces_binary_to_list1(Binary_pieces, []),

    {ok, Pieces}.

pieces_binary_to_list1(<<>>, Acc) ->
    Acc;
pieces_binary_to_list1(<<Piece_bin:20/integer, Rest/binary>>, Acc) ->
    % TODO maybe remove the bin_to_list since string:equals can handle
    % binary-string to string comparision

    % Convert to string
    Piece_str = binary:bin_to_list(Piece_bin),
    pieces_binary_to_list1(Rest, [Piece_str| Acc]).

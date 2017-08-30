-module(ertorrent_utils).

-export([index_list/1,
         create_file_mapping/2,
         read_term_from_file/1,
         write_term_to_file/2,
         encode_hash/1,
         pieces_binary_to_list/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

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


unify_file_list(File_paths) ->
    % Unify the tuple list of files and file sizes
    Result = lists:foldl(fun(FileTuple, Acc) ->
                             case FileTuple of
                                 {File, Length} ->
                                     [{File, Length}| Acc];
                                 {File, Length, _} ->
                                     [{File, Length}| Acc]
                             end
                         end, [], File_paths),

    {ok, lists:reverse(Result)}.

% The peer wire protocol is referring to piece index. This is the total size of
% the download media divided by the piece size. However when the download media
% contains multiple files then the files will be concatenated in the order
% used in the metainfo and then divided by the piece size. File operation are,
% surprisingly, unaware of piece index and instead requires a file path, an
% offset and a byte size. This function will create a list with the mapping
% between piece index and file information.
create_file_mapping(File_paths, Piece_size) ->
    {ok, Unified} = unify_file_list(File_paths),

    Mapping = create_file_mapping1(Unified, Piece_size, 0, []),
    {ok, Mapping}.

% create_file_mapping1/4 is used during even piece conditions
create_file_mapping1([], Piece_size, Current_index, Acc) ->
    Acc;
create_file_mapping1([{File, Length}| Rest_files], Piece_size, Current_index, Acc) ->
    % Calculate the amount of whole pieces
    Piece_num = Length div Piece_size,
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
                                 [{Next_index, File, Next_index * Piece_size, Piece_rem}],
                                 Acc ++ File_mapping);
        true ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 Acc ++ File_mapping)
    end.

% create_file_mapping1/5 is used during uneven piece conditions
create_file_mapping1([], Piece_size, Current_index, Remaining_piece, Acc) ->
    % NOTE Piece_rem already contains the current index
    Acc ++ Remaining_piece;
create_file_mapping1([{File, Length}| Rest_files], Piece_size, Current_index, Remainder, Acc) ->
    % Get the size from the previous piece to calulate the remaining
    % portion of that piece from this file.
    [{_, _, _, Previous_size}] = Remainder,

    % Calculate the size of the first section of this file since there were a
    % remainder of the previous file.
    First_size = Piece_size - Previous_size,

    % Add the size of the remainder to the file size
    New_total_size = Length + Previous_size,

    Piece_num = New_total_size div Piece_size,
    Piece_rem = New_total_size rem Piece_size,

    % Calulate the index which the next recursion should start from
    Next_index = Current_index + Piece_num,

    % Subtract 1 from the next index to stay within the range
    Piece_index_seq = lists:seq(Current_index, Next_index - 1),
    Element_index_seq = lists:seq(0, Piece_num - 1),

    Zipped_indices = lists:zip(Piece_index_seq, Element_index_seq),

    % Creating a list of tuples with the values:
    % {Piece_index, File_path, Offset_in_bytes, Length_in_bytes}
    File_mapping = [{Piece_index, File, Element_index * Piece_size - Previous_size, Piece_size} ||
                    {Piece_index, Element_index} <- Zipped_indices ],

    % Extract the first piece of the file to adjust values
    [{F_piece_index, F_file, F_offset, _}| Rest] = File_mapping,

    % Adjust the offset to 0 and set the size to the previously calculated size
    New_first = {F_piece_index, F_file, 0, First_size},

    % Create a list with the seam between the the previous file and this file
    % and put it together with the file mapping.
    New_file_mapping = [Remainder ++ [New_first]] ++ Rest,

    case Piece_rem =:= 0 of
        false ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 [{Next_index, File, (Next_index - 1) * Piece_size + First_size, Piece_rem}],
                                 Acc ++ New_file_mapping);
        true ->
            create_file_mapping1(Rest_files,
                                 Piece_size,
                                 Next_index,
                                 Acc ++ New_file_mapping)
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

%%% UNIT TESTS %%%

-ifdef(EUNIT).

unify_file_list_output_test() ->
    Input = [{"/home/user/media/foo", 11000},
             {"/home/user/media/bar", 9000, "24ddba2b44f5991b636b04be9ab29535"}],

    Expected_output = [{"/home/user/media/foo", 11000},
                       {"/home/user/media/bar", 9000}],

    {ok, Actual_output} = unify_file_list(Input),

    ?assert(Actual_output =:= Expected_output).

create_file_mapping_output_test() ->
    Input_files = [{"/home/user/media/foo", 11000},
                   {"/home/user/media/bar", 9000, "24ddba2b44f5991b636b04be9ab29535"}],

    Input_piece_size = 2000,

    Expected_output = [{0,"/home/user/media/foo",0,2000},
                       {1,"/home/user/media/foo",2000,2000},
                       {2,"/home/user/media/foo",4000,2000},
                       {3,"/home/user/media/foo",6000,2000},
                       {4,"/home/user/media/foo",8000,2000},
                       [{5,"/home/user/media/foo",10000,1000},{5,"/home/user/media/bar",0,1000}],
                       {6,"/home/user/media/bar",1000,2000},
                       {7,"/home/user/media/bar",3000,2000},
                       {8,"/home/user/media/bar",5000,2000},
                       {9,"/home/user/media/bar",7000,2000}],

    {ok, Actual_output} = create_file_mapping(Input_files, Input_piece_size),
    ?debugFmt("~p~n", [Actual_output]),

    ?assert(Actual_output =:= Expected_output).

-endif.

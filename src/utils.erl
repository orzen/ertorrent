-module(utils).

-export([read_term_from_file/1,
         write_term_to_file/2,
         encode_hash/1,
         unique_id/0]).

ensure_file_entries({files, multiple, Name, Files}, Location) ->
    Dir = Location ++ '/' ++ Name,

    % Make sure that the torrent folder exists
    case ensure_dir(Dir) of 
        ok ->
            % Create files
            foreach(fun(X) ->
                        {ok, Fd} = file:open(Dir ++ '/' ++ X, [write]),
                        % TODO add support for allocate
                        % file:allocate(Fd, Offset, Length)
                        file:close(Fd)
                    end, Files),
    end;
ensure_file_entries({files, single, Name, [File]}, Location) ->
    {ok, Fd} = file:open(Dir ++ '/' ++ Name),
    file:close(Fd).

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

unique_id() ->
    erlang:phash2({node(), now()}).

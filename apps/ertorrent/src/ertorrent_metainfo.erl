-module(ertorrent_metainfo).

-export([get_value/2,
         get_info_value/2,
         is_magnet/1,
         parse_magnet/1,
         parse_file/1,
         resolve_files/1,
         split_pieces/2]).

resolve_files(Metainfo) ->
    {ok, Info} = metainfo:get_value(<<"info">>, Metainfo),
    {ok, Name} = metainfo:get_value(<<"name">>, Info),

    % Determine the file mode and resolve the files into a predicatable and
    % efficient format.
    case metainfo:get_value(<<"files">>, Info) of
        {ok, Files} ->
            File_mode = multiple,

            Resolved_files =
            lists:foldl(fun(X, Acc) ->
                % Mandatory values
                {<<"length">>, Length} = lists:keyfind(<<"length">>, 1, X),
                {<<"path">>, [Path]} = lists:keyfind(<<"path">>, 1, X),

                % Optional value
                case lists:keyfind(<<"md5sum">>, 1, X) of
                    {<<"md5sum">>, Md5sum} ->
                        File = {binary_to_list(Path), Length, Md5sum};
                    false ->
                        File = {binary_to_list(Path), Length}
                end,

                [File| Acc]
            end, [], Files);
        {error, no_match} ->
            File_mode = single,

            % Mandatory value
            {ok, Length} = metainfo:get_value(<<"length">>, Info),

            % Optional value
            case lists:keyfind(<<"md5sum">>, 1, Info) of
                {<<"md5sum">>, Md5sum} ->
                    Resolved_files = [{Length, Md5sum}];
                false ->
                    Resolved_files = [{Length}]
            end
    end,

    {files, File_mode, binary_to_list(Name), Resolved_files}.

% Retrieve value for given key, from the metainfo
get_value(Key, Metainfo) ->
    case lists:keyfind(Key, 1, Metainfo) of
        {_, Value} ->
            {ok, Value};
        false ->
            {error, no_match}
    end.

% Retrieve value for given key, from the info-section of the metainfo
get_info_value(Key, Metainfo) ->
    case lists:keyfind(<<"info">>, 1, Metainfo) of
        {_, Value} ->
            get_value(Key, Value);
        false ->
            {error, no_info_match}
    end.

is_magnet(Str) ->
    case string:str(Str, "magnet:?") == 1 of
        true -> true;
        _ -> false
    end.

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    bencode:decode(Data).

parse_magnet(Uri) ->
    Urn = string:substr(Uri, 9),
    Urn_list = string:tokens(Urn, "&"),
    Parsed = parse_urn(Urn_list, []),
    Decoded = decode_magnet(Parsed),
    {ok, Decoded}.

split_pieces(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
split_pieces(<<Piece:160/integer, Rest/binary>>, Acc) ->
    [Hex] = io_lib:format("~40.16.0b", [Piece]),
    split_pieces(Rest, [Hex | Acc]).

%% INTERNAL FUNCTIONS
parse_urn([], Acc) ->
    lists:reverse(Acc);
parse_urn([H|T], Acc) ->
    {Key, Value} = parse_key_val(H, []),
    Acc2 = [{Key, Value}|Acc],
    parse_urn(T, Acc2).

parse_key_val([], Acc) ->
    {[], Acc};
parse_key_val([$=|Tail], Acc) ->
    {lists:reverse(Acc), Tail};
parse_key_val([H|Tail], Acc) ->
    parse_key_val(Tail, [H|Acc]).

decode_magnet(Magnet) ->
    decode_magnet(Magnet, []).

decode_magnet([], Acc) ->
    lists:reverse(Acc);
decode_magnet([H|Rest], Acc) ->
    {Key, Value} = H,
    {ok, Decoded_key} = uri:decode(Key),
    {ok, Decoded_value} = uri:decode(Value),
    New_acc = [{Decoded_key, Decoded_value}| Acc],
    decode_magnet(Rest, New_acc).

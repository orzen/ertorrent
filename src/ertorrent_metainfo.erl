-module(ertorrent_metainfo).

-export([create_info_hash/1,
         get_value/2,
         get_info_value/2,
         is_magnet/1,
         handle_magnet/1,
         parse_file/1,
         resolve_files/1,
         split_pieces/1]).

-include_lib("kernel/include/logger.hrl").
-include("ertorrent_metainfo.hrl").
-include("ertorrent_modules.hrl").

handle_magnet(Magnet_uri) ->
    {ok, Query} = parse_magnet(Magnet_uri),
    F = fun(X, Acc = #'metainfo.magnet'{announce_list = List}) ->
            case X of
                {"tr", Val} ->
                    List1 = lists:append(List, [Val]),
                    Acc#'metainfo.magnet'{announce_list = List1};
                {"xt", Val} ->
                    #{scheme := "urn", path := Path} = uri_string:parse(Val),
                    ["btih", Info_hash] = string:split(Path, ":"),
                    Acc#'metainfo.magnet'{info_hash = list_to_binary(Info_hash)};
                {"dn", Val} ->
                    Acc#'metainfo.magnet'{name = Val}
            end
        end,
    {ok, lists:foldl(F, #'metainfo.magnet'{}, Query)}.

create_info_hash(Metainfo) ->
    {ok, Info} = get_value(<<"info">>, Metainfo),
    {ok, Info_encoded} = ?BENCODE:encode(Info),
    ?UTILS:hash_digest_to_string(Info_encoded).

resolve_files(Metainfo) ->
    {ok, Info} = get_value(<<"info">>, Metainfo),
    {ok, Name_bin} = get_value(<<"name">>, Info),

    Name = binary_to_list(Name_bin),

    % Determine the file mode and resolve the files into a predicatable and
    % efficient format.
    {File_mode, Resolved_files} = case get_value(<<"files">>, Info) of
        {ok, Files} ->
            Fold = fun(X, Acc) ->
               % Mandatory values
               {<<"length">>, Length} = lists:keyfind(<<"length">>, 1, X),
               {<<"path">>, [Path]} = lists:keyfind(<<"path">>, 1, X),

               % Optional value
               File = case lists:keyfind(<<"md5sum">>, 1, X) of
                   {<<"md5sum">>, Md5sum} ->
                       {binary_to_list(Path), Length, Md5sum};
                   false ->
                       {binary_to_list(Path), Length}
               end,

               [File| Acc]
            end,

            Files2 = lists:foldl(Fold, [], Files),
            {multiple, Files2};
        {error, no_match} ->
            % Mandatory value
            {ok, Length} = get_value(<<"length">>, Info),

            % Optional value
            Files2 = case lists:keyfind(<<"md5sum">>, 1, Info) of
                {<<"md5sum">>, Md5sum} ->
                    [{Name, Length, Md5sum}];
                false ->
                    [{Name, Length}]
            end,
            {single, Files2}
    end,

    {files, File_mode, Name, Resolved_files}.

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
    try ?BENCODE:decode(Data) of
        {ok, Decode} -> {ok, Decode}
    catch
        Throw -> {throw, failed_to_decode, Throw}
    end.

parse_magnet(Uri) ->
    Parsed = uri_string:parse(Uri),
    #{scheme := "magnet", query := Query} = Parsed,
    Dissect = uri_string:dissect_query(Query),
    ?LOG_INFO("dissect '~s'", [Dissect]),
    {ok, Dissect}.

split_pieces(Pieces) ->
    {ok, [io_lib:format("~40.16.0b", [Piece]) || <<Piece:160/integer>> <= Pieces]}.

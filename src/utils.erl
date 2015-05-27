-module(utils).

-export([is_magnet/1, encode_hash/1]).

encode_hash(Info_bencoded) ->
    %160bits/8=20 byte SHA1 as integerlist
    <<Hash:160/integer>> = crypto:hash(sha, Info_bencoded),
    %%Convert the integerlist to a string with len:40, base:16, type:binary
    Info_hash = lists:flatten(io_lib:format("~40.16.0b", [Hash])),
    {ok, format_hash(Info_hash, [])}.

format_hash([], Acc) ->
    lists:reverse(Acc);
format_hash([Msn, Lsn|Tail], Acc) ->
    New_acc = [Lsn, Msn, $%|Acc],
    format_hash(Tail, Reserved_list, New_acc).

is_magnet(Str) ->
    case string:str(Str, "magnet:?") == 1 of
        true -> true;
        false -> false
    end.

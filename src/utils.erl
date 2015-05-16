-module(utils).

-export([is_magnet/1]).

is_magnet(Str) ->
    case string:str(Str, "magnet:?") == 1 of
        true -> true;
        false -> false
    end.

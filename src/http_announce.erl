-module(http_announce).

-include("metainfo.hrl").

-export([do_announce/1]).

do_announce(Metainfo) ->
    #metainfo{announce=Announce_address,
              info_hash=Info_hash,
              info=Info} = Metainfo,
    #info{length=Length} = Info,
    Request = lists:concat([binary_to_list(Announce_address), '?',
                            "info_hash=", Info_hash,
                            "&peer_id=ET",
                            "&port=6882",
                            "&uploaded=0",
                            "&downloaded=0",
                            "&left=", Length,
                            "&event=started"]),
    ok = inets:start(),
    {ok, Result} = httpc:request(get, {Request, []}, [], [{sync, true}]),
    ok = inets:stop(),
    Result.

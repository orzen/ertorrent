-module(http_announce).

-include("metainfo.hrl").

-export([do_announce/1]).

do_announce(Metainfo) ->
    #metainfo{announce=Announce_address,
              info_hash=Info_hash,
              info=Info} = Metainfo,
    #info{length=Length} = Info,
    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    %Replacing reserved characters
    Peer_id_enc = edoc_lib:escape_uri(Peer_id),
    %compact=1 is required by BEP 23:
    %http://www.bittorrent.org/beps/bep_0023.html
    Request = lists:concat([binary_to_list(Announce_address), '?',
                            "info_hash=", Info_hash,
                            "&peer_id=", Peer_id_enc,
                            "&port=6882",
                            "&uploaded=0",
                            "&downloaded=0",
                            "&left=", Length,
                            "&event=started",
                            "&compact=1"]),
    ok = inets:start(),
    {ok, Result} = httpc:request(get, {Request, []}, [], [{sync, true}]),
    ok = inets:stop(),
    Result.

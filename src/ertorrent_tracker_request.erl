-module(ertorrent_tracker_request).

-export([
         new_bin_request/9,
         new_request/8,
         new_request/9
        ]).
new_bin_request(Announce_address, Info_hash, Peer_id, Port, Uploaded,
                Downloaded, Left, Event, Compact) when
     is_binary(Announce_address) andalso
     is_binary(Info_hash) andalso
     is_binary(Peer_id) andalso
     is_binary(Port) andalso
     is_binary(Uploaded) andalso
     is_binary(Downloaded) andalso
     is_binary(Left) andalso
     is_binary(Event) andalso
     is_binary(Compact) ->
    <<Announce_address/binary,
      <<"?info_hash=">>/binary, Info_hash/binary,
      <<"&peer_id=">>/binary, Peer_id/binary,
      <<"&port=">>/binary, Port/binary,
      <<"&uploaded=">>/binary, Uploaded/binary,
      <<"&downloaded=">>/binary, Downloaded/binary,
      <<"&left=">>/binary, Left/binary,
      <<"&event=">>/binary, Event/binary,
      <<"&compact=">>/binary, Compact/binary>>.

new_request(Announce_address, Info_hash, Peer_id, Port, Uploaded, Downloaded,
            Left, Event) ->
    lists:concat([Announce_address, "?", "info_hash=",
                  Info_hash, "&peer_id=", Peer_id, "&port=", Port,
                  "&uploaded=", Uploaded, "&downloaded=", Downloaded, "&left=",
                  Left, "&event=", Event]).

new_request(Announce_address, Info_hash, Peer_id, Port, Uploaded, Downloaded,
            Left, Event, Compact) ->
    lists:concat([Announce_address, "?", "info_hash=",
                  Info_hash, "&peer_id=", Peer_id, "&port=", Port,
                  "&uploaded=", Uploaded, "&downloaded=", Downloaded, "&left=",
                  Left, "&event=", Event, "&compact=", Compact]).

%%%%%%---------------------------------------------------------------------
%%%%%% Description module http_get_request
%%%%%%---------------------------------------------------------------------
%%%%%% This module contains everything necessary to perform a HTTP tracker
%%%%%% GET request and handle its response. This module supports both basic
%%%%%% and compact mode (explained in BEP 23).
%%%%%%
%%%%%% Links:
%%%%%% http://bittorrent.org/beps/bep_0003.html
%%%%%% http://www.bittorrent.org/beps/bep_0023.html
%%%%%%---------------------------------------------------------------------
%%%%%% Exports
%%%%%%---------------------------------------------------------------------
%%%%%% send(Metainfo, Port, Uploaded, Downloaded, Event)
%%%%%%   Returns the response of a performed GET request with the given
%%%%%%   metainfo.
%%%%%%---------------------------------------------------------------------
-module(ertorrent_tracker_protocol).

-export([send/1, new_request/9]).

new_request(Announce_address, Info_hash, Peer_id, Port, Uploaded, Downloaded,
            Left, Event, Compact) ->
    lists:concat([binary_to_list(Announce_address), "?", "info_hash=",
                  Info_hash, "&peer_id=", Peer_id, "&port=", Port,
                  "&uploaded=", Uploaded, "&downloaded=", Downloaded, "&left=",
                  Left, "&event=", Event, "&compact=", Compact]).

send(Request) ->
    inets:start(),
    try
        {ok, {{_, Code, _}, _, Response}} = httpc:request(get,
                                                          {Request,
                                                           [{"Accept",
                                                             "text/plain"}]},
                                                          [],
                                                          [{sync, true},
                                                           {headers_as_is, true}]),
        {ok, Code, Response}
    catch
        Exception:Reason -> {error, Exception, Reason}
    after
        inets:stop()
    end.

-module(udp_messages).

-export([new_connect_request/1,
         parse_connect_response/1,
         new_announce_request/13,
         parse_announce_response/1,
         new_scrape_request/4,
         parse_scrape_response/1,
         parse_error_response/1]).

new_connect_request(Transaction_id) ->
    Connection_id = 16#41727101980,
    Action = 0,

    Request = <<Connection_id:8/big-signed-integer-unit:8,
                Action:4/big-signed-integer-unit:8,
                Transaction_id:4/big-signed-integer-unit:8>>,
    {ok, Request}.

parse_connect_response(Response) ->
    <<Action:4/big-signed-integer-unit:8,
      Transaction_id:4/big-signed-integer-unit:8,
      Connection_id:8/big-signed-integer-unit:8>> = Response,
    {ok, Action, Transaction_id, Connection_id}.

new_announce_request(Connection_id,
                     Action,
                     Transaction_id,
                     Info_hash,
                     Peer_id,
                     Downloaded,
                     Left,
                     Uploaded,
                     Event,
                     Ip_address,
                     Key,
                     Num_want,
                     Port) ->
    Request = <<Connection_id:8/big-signed-integer-unit:8,
                Action:4/big-signed-integer-unit:8,
                Transaction_id:4/big-signed-integer-unit:8,
                Info_hash:20/big-signed-integer-unit:8,
                Peer_id:20/big-signed-integer-unit:8,
                Downloaded:8/big-signed-integer-unit:8,
                Left:8/big-signed-integer-unit:8,
                Uploaded:8/big-signed-integer-unit:8,
                Event:4/big-signed-integer-unit:8,
                Ip_address:4/big-signed-integer-unit:8,
                Key:4/big-signed-integer-unit:8,
                Num_want:4/big-signed-integer-unit:8,
                Port:2/big-signed-integer-unit:8>>,
    {ok, Request}.

parse_announce_response(Response) ->
    <<Action:4/big-signed-integer-unit:8,
      Transaction_id:4/big-signed-integer-unit:8,
      Interval:4/big-signed-integer-unit:8,
      Leechers:4/big-signed-integer-unit:8,
      Seeders:4/big-signed-integer-unit:8,
      Ip_address:4/big-signed-integer-unit:8,
      Port:2/big-signed-integer-unit:8>> = Response,
    {ok, Action, Transaction_id, Interval, Leechers, Seeders, Ip_address, Port}.

new_scrape_request(Connection_id,
                   Action,
                   Transaction_id,
                   Info_hash) ->
    Request = <<Connection_id:8/big-signed-integer-unit:8,
                Action:4/big-signed-integer-unit:8,
                Transaction_id:4/big-signed-integer-unit:8,
                Info_hash:20/big-signed-integer-unit:8>>,
    {ok, Request}.

parse_scrape_response(Response) ->
    <<Action:4/big-signed-integer-unit:8,
      Transaction_id:4/big-signed-integer-unit:8,
      Seeders:4/big-signed-integer-unit:8,
      Completed:4/big-signed-integer-unit:8,
      Leechers:4/big-signed-integer-unit:8>> = Response,
    {ok, Action, Transaction_id, Seeders, Completed, Leechers}.

parse_error_response(Response) ->
    <<Action:4/big-signed-integer-unit:8,
      Transaction_id:4/big-signed-integer-unit:8,
      Message/big-signed-integer>> = Response,
    {ok, Action, Transaction_id, Message}.

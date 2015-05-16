-module(udp_messages).

-export([new_connect_request/3,
         parse_connect_response/1,
         new_announce_request/13,
         parse_announce_response/1,
         new_scrape_request/4,
         parse_scrape_response/1,
         parse_error_response/1]).

new_connect_request(Connection_id,
                    Action,
                    Transaction_id) ->
    Request = <<Connection_id:64/big-signed-integer,
                Action:32/big-signed-integer,
                Transaction_id:32/big-signed-integer>>,
    {ok, Request}.

parse_connect_response(Response) ->
    <<Action:32,
      Transaction_id:32,
      Connection_id:64>> = Response,
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
    Request = <<Connection_id:64,
                Action:32,
                Transaction_id:32,
                Info_hash:160,
                Peer_id:160,
                Downloaded:64,
                Left:64,
                Uploaded:64,
                Event:32,
                Ip_address:32,
                Key:32,
                Num_want:32,
                Port:16>>,
    {ok, Request}.

parse_announce_response(Response) ->
    <<Action:32,
      Transaction_id:32,
      Interval:32,
      Leechers:32,
      Seeders:32,
      Ip_address:32,
      Port:16>> = Response,
    {ok, Action, Transaction_id, Interval, Leechers, Seeders, Ip_address, Port}.

new_scrape_request(Connection_id,
                   Action,
                   Transaction_id,
                   Info_hash) ->
    Request = <<Connection_id:64,
                Action:32,
                Transaction_id:32,
                Info_hash:160>>,
    {ok, Request}.

parse_scrape_response(Response) ->
    <<Action:32,
      Transaction_id:32,
      Seeders:32,
      Completed:32,
      Leechers:32>> = Response,
    {ok, Action, Transaction_id, Seeders, Completed, Leechers}.

parse_error_response(Response) ->
    <<Action:32,
      Transaction_id:32,
      Message>> = Response,
    {ok, Action, Transaction_id, Message}.

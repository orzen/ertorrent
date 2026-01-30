-record(tracker, {
          clients = #{} :: map(),
          id,
          host,
          port,
          proto :: 'udp' | 'http' | 'https',
          inet_version :: 'inet' | 'inet6',
          refs = 0
         }).
-record('tracker.peer', {ip :: tuple(),
                         port :: integer()}).
-record('tracker.peer6', {ip :: tuple(),
                          port :: integer()}).

%%%% HTTP TRACKER WORKER  %%%%

-record('tracker.http.announce', {
          info_hash :: bitstring(),
          peer_id :: bitstring(),
          port :: integer(),
          uploaded = 0 :: integer(),
          downloaded = 0 :: integer(),
          left = 0 :: integer(),
          event = 'stopped' :: 'started' | 'stopped' | 'completed',
          compact = 1 :: integer()
         }).
-record('tracker.http.args', {
          id :: atom(),
          host,
          port,
          tracker_srv,
          tls
         }).
-record('tracker.http.connect', { client_pid, info_hash, reply }).
-record('tracker.http.connected', { client_pid }).
-record('tracker.http.disconnect', { client_pid, reply }).
-record('tracker.http.disconnected', { client_pid }).

%%%% UDP TRACKER WORKER  %%%%

% TODO check if this is still used
-record('tracker.udp.announce', {
          info_hash :: bitstring(),
          peer_id :: bitstring(),
          uploaded = 0 :: integer(),
          downloaded = 0 :: integer(),
          left = 0 :: integer(),
          ip_addr :: integer(),
          event :: integer(),
          key :: integer(),
          num_want,
          port :: integer()
         }).
-record('tracker.udp.args', {
          id :: atom(),
          tracker_srv,
          socket,
          host, % TODO determine if binary is good here
          src_port :: integer(),
          dst_port :: integer(),
          inet_version :: 'inet' | 'inet6'
         }).
-record('tracker.udp.transaction_expired', { transaction_id }).

% UDP tracker messages
-record('tracker.udp.msg.announce_req', {connection_id,
                                         transaction_id,
                                         info_hash :: bitstring(),
                                         peer_id :: bitstring(),
                                         downloaded :: integer(),
                                         left :: integer(),
                                         uploaded :: integer(),
                                         event :: integer(),
                                         ipv4 :: integer(),
                                         key :: integer(),
                                         num_want :: integer(),
                                         port :: integer()}).
-record('tracker.udp.msg.announce_res', {transaction_id,
                                         interval :: integer(),
                                         leechers :: integer(),
                                         seeders :: integer(),
                                         peers :: list(),
                                         peers6 :: list()}).
-record('tracker.udp.msg.connect_req', {protocol_id,
                                        transaction_id}).
-record('tracker.udp.msg.connect_res', {transaction_id,
                                        connection_id}).
-record('tracker.udp.msg.error', {transaction_id,
                                  message :: bitstring()}).
-record('tracker.udp.msg.scrape_req', {connection_id,
                                       transaction_id,
                                       info_hashes :: list()}).
-record('tracker.udp.msg.scrape_res', {transaction_id,
                                       data :: list()}).

%%%% GENERIC TRACKER WORKER  %%%%

% tracker_pid refers to tracker_worker_pid
% data will be a record of the type 'tracker.http.announce' or
% 'tracker.udp.announce' depending on the tracker protocol.
-record('tracker.worker.subscribe', { client_pid, reply }).
-record('tracker.worker.subscribed', {client_pid,
                                      tracker_worker_pid :: atom()}).
-record('tracker.worker.unsubscribe', { client_pid, reply }).
-record('tracker.worker.unsubscribed', { client_pid }).
-record('tracker.worker.peers', { leechers :: integer(),
                                  seeders :: integer(),
                                  peers = [] :: list(),
                                  peers6 = [] :: list() }).
-record('tracker.worker.scrape_data', {data :: list()}).
% Sent from the tracker workers to a torrent worker when it's time to announce
-record('tracker.worker.announce', {tracker_worker_pid :: atom()}).
% Sent from a torrent worker as a response to a tracker worker when it's time
% to announce.
-record('tracker.worker.announce_data', {client_pid :: atom(),
                                         info_hash :: bitstring(),
                                         peer_id :: bitstring(),
                                         downloaded :: integer(),
                                         left :: integer(),
                                         uploaded :: integer(),
                                         event :: atom()}).
% Used internally in a tracker worker process to signal when it's time for a
% client to announce.
-record('tracker.worker.announce_expired', { client_pid }).
%-record('tracker.worker.ready', {tracker_worker_pid :: atom()}).

-record('tracker.srv.args', { tracker_sup_pid }).
-record('tracker.srv.subscribe', { uri, reply }).
-record('tracker.srv.subscribed', { tracker_worker_pid }).
-record('tracker.srv.unsubscribe', { tracker_worker_pid, reply}).
-record('tracker.srv.unsubscribed', { tracker_worker_pid}).

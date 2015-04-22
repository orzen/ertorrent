-record(tracker, {
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
          tracker_srv,
          socket,
          host, % TODO determine if binary is good here
          src_port :: integer(),
          dst_port :: integer(),
          inet_version :: 'inet' | 'inet6'
         }).
-record('tracker.udp.connect', { client_pid, reply }).
-record('tracker.udp.connected', { client_pid }).
-record('tracker.udp.disconnect', { client_pid, reply }).
-record('tracker.udp.disconnected', { client_pid }).
-record('tracker.udp.transaction_expired', { transaction_id }).
-record('tracker.udp.msg.announce_req', {connection_id,
                                         transaction_id,
                                         info_hash :: bitstring(),
                                         peer_id :: bitstring(),
                                         downloaded :: integer(),
                                         left :: integer(),
                                         uploaded :: integer(),
                                         event :: integer(),
                                         ipv4 :: string(),
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
-record('tracker.udp.msg.error_res', {transaction_id,
                                      message :: bitstring()}).
-record('tracker.udp.msg.scrape_req', {connection_id,
                                       transaction_id,
                                       info_hashes :: list()}).
-record('tracker.udp.msg.scrape_res', {transaction_id,
                                       data :: list()}).

%%%% GENERIC TRACKER WORKER  %%%%

% data will be a record of the type 'tracker.http.announce' or
% 'tracker.udp.announce' depending on the tracker protocol.
-record('tracker.worker.client_data', { client_pid, data }).
-record('tracker.worker.data_request', { tracker_pid }).
-record('tracker.worker.peers', { leechers :: integer(),
                                  seeders :: integer(),
                                  peers = [] :: list(),
                                  peers6 = [] :: list() }).
-record('tracker.worker.announce_expired', { client_pid }).

-record('tracker.srv.args', { tracker_sup_pid }).
-record('tracker.srv.connect', { proto, host, port, reply }).
-record('tracker.srv.connected', { tracker_worker_pid }).
-record('tracker.srv.disconnect', { tracker_worker_pid, reply}).
-record('tracker.srv.disconnected', { tracker_worker_pid}).

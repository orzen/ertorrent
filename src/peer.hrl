%% File: peer.hrl
%%
%%-----------------------------
%% Data Type: peer
%% where:
%%     peer_id:
%%     ip:
%%     port:
%%-----------------------------

-record(peer,
        {type,
         id,
         ip,
         port}).

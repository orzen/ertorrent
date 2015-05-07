%% File: magnet_uri.hrl
%%
%%-----------------------------
%% Data Type: magnet_uri
%% where:
%%     content_hash:
%%     name:
%%     url:
%%-----------------------------

-record(magnet_uri,
        {content_hash,
         name,
         url = []}).

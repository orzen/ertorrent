%% File: announce_response.hrl
%%
%%-----------------------------
%% Data Type: announce_response
%% where:
%%     complete:
%%     incomplete:
%%     interval:
%%     peers:
%%-----------------------------

-record(announce_response,
        {complete,
         incomplete,
         interval,
         peers = []}).

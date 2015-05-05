-include("info.hrl").

%% File: torrent.hrl
%%
%%-----------------------------
%% Data Type: torrent
%% where:
%%     announce:
%%     announce_list:
%%     comment:
%%     creation_date:
%%     httpseeds:
%%     info:
%%-----------------------------

-record(torrent,
        {announce,
         announce_list,
         comment,
         creation_date,
         httpseeds,
         info = #info{}}).

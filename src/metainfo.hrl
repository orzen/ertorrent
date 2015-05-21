-include("info.hrl").

%% File: metainfo.hrl
%%
%%-----------------------------
%% Data Type: metainfo
%% where:
%%     announce:
%%     announce_list:
%%     comment:
%%     creation_date:
%%     httpseeds:
%%     info_hash:
%%     info:
%%-----------------------------

-record(metainfo,
        {announce,
         announce_list,
         comment,
         creation_date,
         httpseeds,
         info_hash,
         info = #info{}}).

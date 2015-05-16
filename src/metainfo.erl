-module(torrent).
-include("torrent.hrl").

-export([get_data/1]).

from_file(Filename) ->
    {{dict, Total}, _} = bencode:decode(Filename),
    parse_torrent(Total, #torrent{}).

parse_torrent([], Record) ->
    Record;
parse_torrent([{<<"announce">>, Value}|Tail], Record) ->
    New_record = Record#torrent{announce=Value},
    parse_torrent(Tail, New_record);
%% This may recieve a nested list-tuple
%% depending on torrent distributer.
parse_torrent([{<<"announce-list">>, Value}|Tail], Record) ->
    New_record = Record#torrent{announce_list=Value},
    parse_torrent(Tail, New_record);
parse_torrent([{<<"comment">>, Value}|Tail], Record) ->
    New_record = Record#torrent{comment=Value},
    parse_torrent(Tail, New_record);
parse_torrent([{<<"creation date">>, Value}|Tail], Record) ->
    New_record = Record#torrent{creation_date=Value},
    parse_torrent(Tail, New_record);
parse_torrent([{<<"httpseeds">>, Value}|Tail], Record) ->
    New_record = Record#torrent{httpseeds=Value},
    parse_torrent(Tail, New_record);
parse_torrent([{<<"info">>, Value}|Tail], Record) ->
    {dict, Info_data} = Value,
    Info_record = parse_info(Info_data, #info{}),
    New_record = Record#torrent{info=Info_record},
    parse_torrent(Tail, New_record).

parse_info([], Record) ->
    Record;
parse_info([{<<"length">>, Value}|Tail], Record) ->
    New_record = Record#info{length=Value},
    parse_info(Tail, New_record);
parse_info([{<<"name">>, Value}|Tail], Record) ->
    New_record = Record#info{name=Value},
    parse_info(Tail, New_record);
parse_info([{<<"piece length">>, Value}|Tail], Record) ->
    New_record = Record#info{piece_length=Value},
    parse_info(Tail, New_record);
parse_info([{<<"pieces">>, Value}|Tail], Record) ->
    New_record = Record#info{pieces=Value},
    parse_info(Tail, New_record).

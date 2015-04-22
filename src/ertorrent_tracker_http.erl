-module(ertorrent_tracker_http).

-export([
         announce_query/1,
         parse_res/1
        ]).

-include_lib("eunit/include/eunit.hrl").
-include("ertorrent_tracker.hrl").

announce_query(#'tracker.http.announce'{info_hash = Info_hash,
                                        peer_id = Peer_id,
                                        port = Port,
                                        uploaded = Uploaded,
                                        downloaded = Downloaded,
                                        left = Left,
                                        event = Event,
                                        compact = Compact}) ->
    Q = ["?info_hash=", binary_to_list(Info_hash),
         "&peer_id=", binary_to_list(Peer_id),
         "&port=", integer_to_list(Port),
         "&uploaded=", integer_to_list(Uploaded),
         "&downloaded=", integer_to_list(Downloaded),
         "&left=", integer_to_list(Left),
         "&event=", atom_to_list(Event)],

    Q2 = case Compact of
        1 -> Q ++ ["&compact=1"];
        0 -> Q
    end,

    Q3 = unicode:characters_to_list(Q2, latin1),

    {ok, Q3}.

% TODO implement this
parse_res(_Res) ->
    ok.

-ifdef(EUNIT).
-define(TEST_INFO_HASH, <<"c12fe1c06bba254a9dc9">>).
-define(TEST_PEER_ID, <<"ET001-abcdefghijklmn">>).

announce_query_compact_test() ->
    I = #'tracker.http.announce'{info_hash = ?TEST_INFO_HASH,
                                 peer_id = ?TEST_PEER_ID,
                                 port = 4242,
                                 uploaded = 1,
                                 downloaded = 2,
                                 left = 3,
                                 event = started},
    Expected = "?info_hash=c12fe1c06bba254a9dc9&peer_id=ET001-abcdefghijklmn&port=4242&uploaded=1&downloaded=2&left=3&event=started&compact=1",

    {ok, Actual} = announce_query(I),
    ?assertEqual(Expected, Actual).

announce_query_non_compact_test() ->
    I = #'tracker.http.announce'{info_hash = ?TEST_INFO_HASH,
                                 peer_id = ?TEST_PEER_ID,
                                 port = 4242,
                                 uploaded = 1,
                                 downloaded = 2,
                                 left = 3,
                                 event = stopped,
                                 compact = 0},
    Expected = "?info_hash=c12fe1c06bba254a9dc9&peer_id=ET001-abcdefghijklmn&port=4242&uploaded=1&downloaded=2&left=3&event=stopped",

    {ok, Actual} = announce_query(I),
    ?assertEqual(Expected, Actual).

-endif.

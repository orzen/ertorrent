# ertorrent

This is currently an early stage development project of an open
source torrent client written in erlang.

## Build instructions

We use rebar to build the project.

    $ rebar compile

You can run the tests with

    $ rebar eunit

## Contribute

### Message tagging

Two-way communication:
<sender (module)>_<s(erver)|w(orker)>_<message name>_<res(ponse)|req(uest)>
Example: file_w_read_offset_res

One-way communication:
<sender (module)>_<s(erver)|w(orker)>_<message name>
Example: peer_w_terminate

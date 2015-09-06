%%%%%%---------------------------------------------------------------------
%%%%%% Description module torrent
%%%%%%---------------------------------------------------------------------
%%%%%% Foobars are the basic elements in the Baz signalling system. The
%%%%%% functions below are for manipulating that data of foobars and for
%%%%%% {state, metainfo, port, uploaded, downloaded, socket}
%%%%%%---------------------------------------------------------------------
%%%%%% Exports
%%%%%%---------------------------------------------------------------------
%%%%%% create_foobar(Parent, Type)
%%%%%%   returns a new foobar object
%%%%%%   etc etc etc
%%%%%%---------------------------------------------------------------------
-module(torrent).

-export([init/0]).

init() ->
    spawn(fun() -> loop0() end).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

save(Data) ->
    io:format("Saving state..."),
    Filename = filename:absname(Meta#metainfo.info_hash),
    utils:write_term_to_file(Filename, Meta).

load(Filename) ->
    io:format("Loading state..."),
    {ok, Data} = utils:read_term_from_file(Filename),
    Data.

new(Metainfo_source) ->
    io:format("Parsing..."),
    case utils:is_magnet(Metainfo_source) of
        true ->
            {ok, {magnet, Meta}} = metainfo:parse_magnet(Metainfo_source);
        false ->
            {ok, {torrent, Meta}} = metainfo:parse_file(Metainfo_source)
    end,
    save(Meta).

start({Metainfo, 0, 0}, Port) ->
    Announce_response = http_get_request:send(Metainfo).
    #metainfo{info=Info} = Metainfo,
    #info{piece_length=Piece_length, pieces=Pieces, length=Length} = Info,

stop() ->
    {ok, stopped}.

close() ->
    {ok}.

destroy() ->
    exit("Removed torrent").

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop0() ->
    receive
        {From, {new, Meta_source}} when State =:= none ->
            io:format("Received: new"),
            {ok, Meta} = new(Meta_source),
            loop({stopped, Meta});
        {From, {load, Filename}} ->
            load(Filename)
    end.

loop(Persistent, Temporary) ->
    receive
        {From, {start, Port}} when State =:= {stopped, Meta} andalso Socket =:= none ->
            io:format("Received: start"),
            New_socket = start(Peer_ip, Peer_port, Info_hash),
            loop({started, Meta}, New_socket);
        {From, stop} ->
            io:format("Received: stop"),
            stop()
    end.

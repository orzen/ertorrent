-module(peer).

-export([loop/0]).

parse_message(B) ->
    {ok, Message} = parse_message(B, []);
parse_message(<<ID:4>>, Ack) ->


do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            parse_message(B);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.

start(Peer_ip, Peer_port, Info_hash) ->
    try gen_tcp:connect(Peer_ip, Peer_port, [binary, {active, true}]) of
        {ok, Socket} -> {started, Socket}
    catch
        {error, Reason} -> {error, Reason}
    end.

stop() ->
    {ok, stopped}.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(Socket) ->
    receive
        {From, {start, Peer_ip, Peer_port, Info_hash}} when Socket =:= {} ->
            New_socket = start(Peer_ip, Peer_port, Info_hash);
            loop(Socket)
        {From, {stop}} ->
            stop()
    end.

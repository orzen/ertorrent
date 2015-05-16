-module(udp_connection).

-export([new/3]).

new(Hash, Tracker_dns, Port) ->
    %{ok, Ip_address} = inet:getaddr(Tracker_dns, inet),
    {ok, Socket} = gen_udp:open(33000),
    {_, _, Transaction_id} = random:seed(erlang:now()),
    {ok, Request} = udp_messages:new_connect_request(16#41727101980, 0, Transaction_id),
    ok = gen_udp:send(Socket, Tracker_dns, Port, Request),
    Response = gen_udp:recv(Socket, 128, 5000),
    gen_udp:close(Socket),
    Response.


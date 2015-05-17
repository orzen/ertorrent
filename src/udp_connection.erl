-module(udp_connection).

-export([new/3]).

new(Hash, Tracker_dns, Port) ->
    {ok, Socket} = gen_udp:open(33000, [{active, false}]),
    {_, _, Transaction_id} = random:seed(erlang:now()),
    {ok, Request} = udp_messages:new_connect_request(16#41727101980, 0, Transaction_id),
    ok = gen_udp:send(Socket, Tracker_dns, Port, Request),
    %How is Response kept as binary instead of converted to int list?
    {ok, {_, _, Response}} = gen_udp:recv(Socket, 128, 5000),
    Response2 = udp_messages:parse_connect_response(iolist_to_binary(Response)),
    gen_udp:close(Socket),
    {transaction_id, Transaction_id, resp, Response2}.

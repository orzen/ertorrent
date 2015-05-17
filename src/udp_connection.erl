-module(udp_connection).

-export([new/3]).

new(Tracker_dns, Port) ->
    {ok, Socket} = gen_udp:open(33000, [{active, false}]),
    {_, _, Transaction_id} = random:seed(erlang:now()),
    %% connection_id:
    %% 0x41727101980 is the default value for new connect requests.
    %% action:
    %% 0 represents connect.
    {ok, Request} = udp_messages:new_connect_request(Transaction_id),
    ok = gen_udp:send(Socket, Tracker_dns, Port, Request),
    %How is Response kept as binary instead of converted to int list?
    {ok, {_, _, Response}} = gen_udp:recv(Socket, 128, 5000),
    Response2 = udp_messages:parse_connect_response(iolist_to_binary(Response)),
    gen_udp:close(Socket),
    {transaction_id, Transaction_id, resp, Response2}.

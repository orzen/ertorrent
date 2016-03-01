%%%-------------------------------------------------------------------
%%% @author Martin & Eric <erlware-dev@googlegroups.com>
%%% [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TCP server. This module defines a server process that
%%% listens for incoming TCP connections and allows the user to
%%% execute RPC commands via that TCP stream.
%%% @end
%%%-------------------------------------------------------------------

-module(torrent_gen).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start/0,
         stop/1]).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {metainfo,
                socket}).

%%% Standard client API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

call(ServerRef, Request) ->
    %term().
    ok.

multi_call(Name, Request) ->
    %{Replies, BadNodes}
    ok.

cast(ServerRef, Request) ->
    ok.

abcast(Name, Request) ->
    abcast.

%%% Callback module
init([]) ->
    {ok, #state{}}.

terminate(_Reason, _State) ->
    {ok}.

%% Synchronous
handle_call({init}, From, State) ->
    io:format("~p starting~n",[?MODULE]),
    {ok};
handle_call({start}, From, State) ->
    io:format("~p starting~n",[?MODULE]),
    {ok};
handle_call({stop}, From, State) ->
    io:format("~p stopping~n",[?MODULE]),
    {ok};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% Asynchronous
handle_cast(Request, State) ->
    {ok}.

%% Do work here
handle_info(Info, State) ->
    #state{socket=Socket} = State,
    %case gen_tcp:recv(Socket, 0) of
    %    {error, closed} ->
    %        {ok, list_to_binary(Bs)};
    %    {ok, Packet} ->
    %        io:format("recv: ~w\n", [Packet])
    %end.
    ok.

code_change(OldVsn, State, Extra) ->
    {ok}.


-ifdef(TEST).

dev_test() ->
    erlang:display("Running developer test"),
    {ok, Meta} = metainfo:read_file("../src/debian-8.3.0-amd64-netinst.iso.torrent"),
    io:format("meta: ~p~n", [Meta]),
    {ok, Pid} = torrent_gen:start_link(),
    {ok, Pid2} = torrent_gen:start_link(),
    erlang:display(Pid),
    erlang:display(Pid2),
    Ret = torrent_gen:stop(Pid),
    torrent_gen:stop(Pid2),
    erlang:display(Ret),
    ?assert(true).

-endif.

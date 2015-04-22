-module(ertorrent_tracker_sup).

-behaviour(supervisor).

-export([
         start_child/1,
         terminate_child/1
        ]).

-export([
         start_link/0,
         init/1
        ]).

-include("ertorrent_tracker.hrl").

-define(SERVER, {local, ?MODULE}).

-define(HTTP_WORKER(ID, Args),
    #{
      id => ID,
      start => {ertorrent_tracker_http_worker, start_link, [Args]},
      restart => transient,
      shutdown => 60*1000,
      type => worker,
      modules => [ertorrent_tracker_http_worker]
     }).

-define(UDP_WORKER(ID, Args),
    #{
      id => ID,
      start => {ertorrent_tracker_udp_worker, start_link, [Args]},
      restart => transient,
      shutdown => 60*1000,
      type => worker,
      modules => [ertorrent_tracker_udp_worker]
     }).

open_udp_socket(Src_port) ->
    case gen_udp:open(Src_port, [inet, binary]) of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            lager:error("failed to open UDP socket for tracker communication: '~w'", [Reason]),
            {error, Reason}
    end.

transfer_udp_ownership(Socket, Pid) ->
    case gen_udp:controlling_process(Socket, Pid) of
        ok ->
            lager:debug("transfered ownership of socket"),
            ok;
        {error, Reason} ->
            lager:error("failed to transfer socket ownership to tracker worker"),
            {error, Reason}
    end.

start_child(#tracker{id = ID, proto = udp, host = Host, port = Port}) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),
    % TODO figure out how to handle the local ports
    % TODO figure out how to handle ipv4 and ipv5
    Src_port = 55544,
    {ok, Socket} = open_udp_socket(Src_port),

    Args = #'tracker.udp.args'{host = Host,
                               dst_port = Port,
                               inet_version = inet,
                               socket = Socket,
                               src_port = Src_port,
                               tracker_srv = self()},
    {ok, Pid} = supervisor:start_child(?MODULE, ?UDP_WORKER(ID, Args)),

    ok = transfer_udp_ownership(Socket, Pid),

    {ok, Pid};
start_child(#tracker{id = ID, proto = http, host = Host, port = Port}) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),
    Args = #'tracker.http.args'{
                                host = Host,
                                port = Port,
                                tracker_srv = self(),
                                tls = false
                               },
    supervisor:start_child(?SERVER, ?HTTP_WORKER(ID, Args));
start_child(#tracker{id = ID, proto = https, host = Host, port = Port}) ->
    lager:debug("~p: ~p", [?MODULE, ?FUNCTION_NAME]),
    Args = #'tracker.http.args'{
                                host = Host,
                                port = Port,
                                tracker_srv = self(),
                                tls = true
                               },
    supervisor:start_child(?SERVER, ?HTTP_WORKER(ID, Args)).

terminate_child(#tracker{id = ID}) ->
    case supervisor:terminate_child(?SERVER, ID) of
        ok -> ok;
        {error, Reason} ->
            lager:error("~p:~p failed to terminate tracker worker '~p'",
                        [?MODULE, ?FUNCTION_NAME, Reason]),
            error
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    lager:info("~s(~w): started", [?MODULE, self()]),
    SupFlags = {one_for_one, 1, 1},

    %HTTP_worker_spec = #{
    %  id => ertorrent_tracker_http_worker,
    %  start => {ertorrent_tracker_http_worker, start_link, []},
    %  restart => transient,
    %  shutdown => 60*1000,
    %  type => worker,
    %  modules => [ertorrent_tracker_http_worker]
    % },

    %UDP_worker_spec = #{
    %  id => ertorrent_tracker_udp_worker,
    %  start => {ertorrent_tracker_udp_worker, start_link, []},
    %  restart => transient,
    %  shutdown => 60*1000,
    %  type => worker,
    %  modules => [ertorrent_tracker_udp_worker]
    % },

    % TODO remove if map specs works
    %UDP_worker_specs = {ertorrent_tracker_udp_worker,
    %                    {ertorrent_tracker_udp_worker, start_link, []},
    %                    transient, 60*1000, worker, [ertorrent_tracker_udp_worker]},

    %HTTP_worker_specs = {ertorrent_tracker_http_worker,
    %                     {ertorrent_tracker_http_worker, start_link, []},
    %                     transient, 60*1000, worker, [ertorrent_tracker_http_worker]},

    {ok, {SupFlags, []}}.

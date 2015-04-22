-module(ertorrent_torrent_sup).

-behaviour(supervisor).

-export([start_child/2,
         start_link/0,
         init/1]).

start_child(ID, Args) ->
    lager:debug("~p: ~p: id: '~p'", [?MODULE, ?FUNCTION_NAME, ID]),
    supervisor:start_child(?MODULE, [ID, Args]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {simple_one_for_one, 1, 1},

    Torrent_worker = {ertorrent_torrent_worker,
                      {ertorrent_torrent_worker, start_link, []},
                      transient, 60*1000, worker, [ertorrent_torrent_worker]},

    {ok, {SupFlags, [Torrent_worker]}}.

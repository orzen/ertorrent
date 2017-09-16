-module(ertorrent_torrent_sup).

-behaviour(supervisor).

-export([start_child/2,
         start_link/0,
         init/1]).

-define(CHILD(ID, Args), #{id => ID,
                           start => {ertorrent_torrent_worker, start_link, [ID, Args]},
                           restart => permanent,
                           shutdown => 60*1000,
                           type => worker,
                           modules => [ertorrent_torrent_worker]}).

start_child(ID, Args) ->
    supervisor:start_child(?MODULE, ?CHILD(ID, Args)).

start_link() ->
    supervisor:start_link({local, ertorrent_torrent_sup}, ?MODULE, []).

init(_Arg) ->
    SupFlags = {one_for_one, 10, 10},

    %Child_spec = #{id => ertorrent_torrent_worker,
    %               start => {ertorrent_torrent_worker, start_link, []},
    %               restart => permanent,
    %               shutdown => 2000,
    %               type => worker,
    %               modules => [ertorrent_torrent_worker]},

    {ok, {SupFlags, []}}.

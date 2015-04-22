-module(ertorrent_sup).

-export([start_link/0,
         init/1]).

-include("ertorrent_torrent.hrl").

-define(SUPERVISOR(Module), {Module,
                             {Module, start_link, []},
                             permanent, infinity, supervisor, [Module]}).

-define(SERVER(Module), {Module,
                         {Module, start_link, []},
                         transient, infinity, worker, [Module]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Arg) ->
    Max_restart = 10,
    Max_time = 10,

    Sup_flags = {one_for_one, Max_restart, Max_time},

    Supervisors = [
                   ertorrent_tracker_sup
                  ],
    %Supervisors = [
    %               ertorrent_peer_sup,
    %               ertorrent_peer_statem_sup,
    %               ertorrent_torrent_sup,
    %               ertorrent_file_sup
    %              ],

    Supervisor_specs = [?SUPERVISOR(X) || X <- Supervisors],

    Servers = [
               ertorrent_tracker_srv,
               ertorrent_torrent_worker2
              ],
    %Servers = [
    %           ertorrent_settings_srv,
    %           ertorrent_file_srv,
    %           ertorrent_hash_srv,
    %           ertorrent_tracker_http_dispatcher,
    %           ertorrent_peer_accept,
    %           ertorrent_peer_srv,
    %           ertorrent_torrent_srv
   %          ],

    Server_specs = [?SERVER(X) || X <- Servers],

    Specs = lists:merge(Supervisor_specs, Server_specs),

    {ok, {Sup_flags, Specs}}.

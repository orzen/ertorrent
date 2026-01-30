-module(ertorrent_torrent_worker2).
-behaviour(gen_server).

-export([
         start_link/0,
         init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-include_lib("kernel/include/logger.hrl").
-include("ertorrent_internal.hrl").
-include("ertorrent_modules.hrl").
-include("ertorrent_metainfo.hrl").
-include("ertorrent_torrent.hrl").
-include("ertorrent_tracker.hrl").


-record(state, {
          id :: atom(),
          metainfo,
          trackers = [],
          event = 'stopped' :: 'stopped' | 'started' | 'completed',
          stats = {0,0,0} :: tuple() % {downloaded, left, uploaded}
         }).

tracker(URI) ->
    Args = #'tracker.srv.subscribe'{uri = URI,
                                    reply = l_reply()},
    ?TRACKER_SRV:subscribe(Args).

% start_link([Args = #'torrent.worker.args'{ pname = PName }]) ->
start_link() ->
    M2 = "magnet:?xt=urn:btih:55c3737267ac920ed18eca2fb1a94ddf18a12397&dn=Eminem+-+Kamikaze+%282018%29+Mp3+%28320kbps%29+%5BHunter%5D&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80&tr=udp%3A%2F%2Fopen.demonii.com%3A1337&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969&tr=udp%3A%2F%2Fexodus.desync.com%3A6969",
    {ok, M = #'metainfo.magnet'{info_hash = IH, announce_list = [T| _AL]}} = ?METAINFO:handle_magnet(M2),
    lager:info("~s(~p): info hash '~s', tracker '~s'", [?MODULE, self(), IH, T]),

    Args = #'torrent.worker.args'{id = foo, metainfo = M},
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

init([#'torrent.worker.args'{id = ID,
                             metainfo = Metainfo}]) ->
    lager:info("~s(~w): started", [?MODULE, self()]),

    #'metainfo.magnet'{info_hash = _Info_hash,
                       announce_list = [T| _Rest]} = Metainfo,

    % TODO start all the trackers related to the torrent
    % Start tracker
    tracker(T),

    {ok, #state{id = ID, metainfo = Metainfo}, hibernate}.

terminate(Reason, _State) ->
    lager:info("~s(~p): ~s '~p'", [?MODULE, self(), ?FUNCTION_NAME, Reason]),
    %?TRACKER_SRV:unsubscribe(),
    ok.

handle_call(Req, From, State) ->
    ?LOG_ERROR("~s(~w): unhandled call '~w', '~w'",
               [?MODULE, self(), From, Req]),
    {stop, unhandled_call, State}.

handle_cast(Req, State) ->
    ?LOG_ERROR("~s(~w): unhandled cast '~w'", [?MODULE, self(), Req]),
    {stop, unhandled_cast, State}.

handle_info(#'tracker.worker.announce'{tracker_worker_pid = Tracker},
            State = #state{id = ID,
                           metainfo = Metainfo,
                           stats = {Downloaded, Left, Uploaded}}) ->
    IH = case Metainfo of
        #'metainfo.magnet'{info_hash = Info_hash} -> Info_hash;
        #'metainfo.file'{info_hash = Info_hash} -> Info_hash
    end,

    {ok, Peer_id} = ?SETTINGS_SRV:get_conf(peer_id),

    Tracker ! #'tracker.worker.announce_data'{client_pid = ID,
                                              info_hash = IH,
                                              peer_id = Peer_id,
                                              downloaded = Downloaded,
                                              left = Left,
                                              uploaded = Uploaded,
                                              event = stopped},
    {noreply, State, hibernate};
handle_info(#'tracker.srv.subscribed'{tracker_worker_pid = Pid},
            State = #state{trackers = Trackers}) ->
    {noreply, State#state{trackers = [Pid| Trackers]}, hibernate};
handle_info(Req, State) ->
    lager:error("~s(~w): unhandled info '~w'", [?MODULE, self(), Req]),
    {stop, unhandled_info, State}.

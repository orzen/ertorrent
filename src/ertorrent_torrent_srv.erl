%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_srv).

-behaviour(gen_server).

-export([start_link/0,
         stop/0,
         add/2,
         member_by_info_hash/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(BENCODE, ertorrent_bencode).
-define(ERTORRENT_META_FILE, "TEST_FILE").
-define(METAINFO, ertorrent_metainfo).
-define(TORRENT_SUP, ertorrent_torrent_sup).
-define(UTILS, ertorrent_utils).

-record(state, {torrents=[],
                torrent_sup,
                torrent_workers=[]}).

%%% Client API

add(Metainfo, Start_when_ready) ->
    gen_server:call(?MODULE, {torrent_s_add_torrent, Metainfo, Start_when_ready}).

member_by_info_hash(Info_hash) ->
    gen_server:call(?MODULE, {torrent_s_member_info_hash, Info_hash}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Internal functions

spawn_workers_from_cache(Torrents) ->
    Foldl = fun({Info_hash, Metainfo}, Acc) ->
                ID = list_to_atom(Info_hash),
                ?TORRENT_SUP:start_child(ID, [Info_hash, Metainfo]),
                [ID| Acc]
            end,
    lists:foldl(Foldl, [], Torrents).

%% Callback functions
init(_Args) ->
    case file:read_file(?ERTORRENT_META_FILE) of
        {ok, Stored_torrent_meta} ->
            Torrents = erlang:binary_to_term(Stored_torrent_meta),

            Torrent_workers = spawn_workers_from_cache(Torrents);
        {error, enoent} ->
            Torrents = [],
            Torrent_workers = [];
        {error, Reason} ->
            lager:warning("unhandled error reason when reading stored torrent meta: '~p'", [Reason]),
            Torrents = [],
            Torrent_workers = []
    end,

    {ok, #state{torrents=Torrents,
                torrent_sup=?TORRENT_SUP,
                torrent_workers=Torrent_workers}, hibernate}.

handle_call({torrent_srv_member_info_hash, Info_hash}, _From, State) ->
    Member = lists:keymember(Info_hash, 1, State),
    {reply, Member, State};

handle_call({torrent_s_add_torrent, Metainfo, Start_when_ready}, _From, State) ->
    % Creating info hash
    {ok, Info} = ?METAINFO:get_value(<<"info">>, Metainfo),
    {ok, Info_encoded} = ?BENCODE:encode(Info),
    {ok, Info_hash} = ?UTILS:hash_digest_to_string(Info_encoded),

    % Preparing torrent tuple
    Torrent = {Info_hash, Metainfo},
    Current_torrents = State#state.torrents,

    Torrent_id = list_to_atom(Info_hash),

    case ?TORRENT_SUP:start_child(Torrent_id, [Metainfo, Start_when_ready]) of
        {ok, _Child} ->
            % Adding torrent tuple to the bookkeeping list
            New_state = State#state{torrents = [Torrent|Current_torrents]},

            % Write the updated bookkeeping list to file
            % TODO commenting this one for now
            %?UTILS:write_term_to_file(?ERTORRENT_META_FILE, New_state#state.torrents),

            Reply = {ok, Info_hash};
        {error, Reason} ->
            New_state = State,

            case Reason of
                already_present ->
                    Reply = {not_ok, already_present};
                {already_present, _Child} ->
                    Reply = {not_ok, already_present};
                Reason ->
                    lager:warning("~p: unexpected error when starting torrent worker: '~p'", [?MODULE, Reason]),
                    Reply = {not_ok, unexpected, Reason}
            end
    end,

    {reply, Reply, New_state, hibernate}.


% @doc API to start added torrents
% @end
handle_cast({start}, State) ->
    io:format("~p starting~n",[?MODULE]),
    {noreply, State};

handle_cast({remove}, State) ->
    io:format("~p remove~n",[?MODULE]),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

%% Do work here
handle_info({peer_s_read_piece, From, Info_hash, Piece_idx}, State) ->
    case lists:keymember(Info_hash, 1, State#state.torrents) of
        true ->
            Info_hash ! {torrent_s_read_piece,
                         From,
                         Info_hash,
                         Piece_idx};
        false ->
            ?WARNING("trying to read piece for an nonexisting torrent")
    end,
    {noreply, State};
handle_info({'EXIT', _ParentPid, shutdown}, State) ->
    {stop, shutdown, State};
handle_info(Info, State) ->
    ?WARNING("unhandled info request: " ++ Info),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p: going down, Reason: ~p~n", [?MODULE, Reason]),
    error_logger:info_msg("~p: terminating, reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

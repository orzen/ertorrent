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
         add/1,
         member_by_info_hash/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("ertorrent_log.hrl").

-define(BENCODE, ertorrent_bencode).
-define(ERTORRENT_META_FILE, "TEST_FILE").
-define(METAINFO, ertorrent_metainfo).
-define(TORRENT_SUP, ertorrent_torrent_sup).
-define(UTILS, ertorrent_utils).

-record(state, {torrents=[],
                torrent_sup}).

%%% Client API

add(Metainfo) ->
    gen_server:call(?MODULE, {torrent_s_add_torrent, Metainfo}).

member_by_info_hash(Info_hash) ->
    gen_server:call(?MODULE, {torrent_s_member_info_hash, Info_hash}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Internal functions

spawn_workers_from_cache(Torrent_sup, Torrents) ->
    lists:foreach(
        fun({Info_hash, Metainfo}) ->
            supervisor:start_child(Torrent_sup,
                                   [list_to_atom(Info_hash),
                                    [Info_hash, Metainfo]])
        end,
        Torrents).

%% Callback functions
init(_Args) ->
    case file:read_file(?ERTORRENT_META_FILE) of
        {ok, Stored_torrent_meta} ->
            Torrents = erlang:binary_to_term(Stored_torrent_meta),

            spawn_workers_from_cache(?TORRENT_SUP, Torrents);
        {error, enoent} ->
            Torrents = [];
        {error, Reason} ->
            ?WARNING("unhandled error reason when reading stored torrent meta: " ++ Reason),
            Torrents = []
    end,

    {ok, #state{torrents=Torrents,
                torrent_sup=?TORRENT_SUP}}.

handle_call({torrent_srv_member_info_hash, Info_hash}, _From, State) ->
    Member = lists:keymember(Info_hash, 1, State),
    {reply, Member, State};

handle_call({torrent_s_add_torrent, Metainfo}, From, State) ->
    % Creating info hash
    {ok, Info} = ?METAINFO:get_value(<<"info">>, Metainfo),
    {ok, Info_encoded} = ?BENCODE:encode(Info),
    {ok, Info_hash} = ?UTILS:encode_hash(Info_encoded),

    % Preparing torrent tuple
    Torrent = {Info_hash, Metainfo},
    Current_torrents = State#state.torrents,

    Torrent_ID = list_to_atom(Info_hash),
    Start_when_ready = false,
    case ertorrent_torrent_sup:start_child(Torrent_ID, [Info_hash, Metainfo, Start_when_ready]) of
        {ok, _Child} ->
            % Adding torrent tuple to the bookkeeping list
            New_state = State#state{torrents = [Torrent|Current_torrents]},

            % Write the updated bookkeeping list to file
            ?UTILS:write_term_to_file(?ERTORRENT_META_FILE, New_state#state.torrents),

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

    {reply, From, Reply, New_state}.


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

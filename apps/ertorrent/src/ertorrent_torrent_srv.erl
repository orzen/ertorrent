%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_srv).

-behaviour(gen_server).

-include("ertorrent_log.hrl").

-define(TORRENTS_FILENAME, "TEST_FILE").
-define(TORRENT_SUP, ertorrent_torrent_sup).

-export([start_link/1,
         stop/0,
         add/1,
         list/1,
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

-record(state, {torrents=[],
                torrent_sup}).

add(Metainfo) ->
    gen_server:call(?MODULE, {add, Metainfo}).

list(Pid) ->
    gen_server:call(Pid, {list}).

print_list([]) ->
    true;
print_list([H|T]) ->
    {Hash, _} = H,
    io:format("~p~n", [Hash]),
    print_list(T).

member_by_info_hash(Info_hash) ->
    gen_server:call({torrent_srv_member_info_hash, Info_hash}).

%%% Standard client API
%%% Cached_metainfo list with metainfo
start_link(Cached_metainfo) ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [{metainfo_cache, Cached_metainfo}],
                          []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

spawn_workers_from_cache(Torrent_sup, Cached_torrents) ->
    lists:foreach(
        fun({Info_hash, Metainfo}) ->
            supervisor:start_child(Torrent_sup,
                                   [list_to_atom(Info_hash),
                                    [Info_hash, Metainfo]])
        end,
        Cached_torrents).

%% Callback module

%%% Init for development
init({metainfo_cache, _Cached_metainfo}) ->
    case filelib:is_file(?TORRENTS_FILENAME) of
        true ->
            {ok, Torrents} = utils:read_term_from_file(?TORRENTS_FILENAME),
            spawn_workers_from_cache(?TORRENT_SUP, Torrents);
        false ->
            Torrents = []
    end,
    {ok, #state{torrents=Torrents,
                torrent_sup=?TORRENT_SUP}}.

%% Synchronous
handle_call({torrent_srv_member_info_hash, Info_hash}, _From, State) ->
    Member = lists:keymember(Info_hash, 1, State),
    {reply, Member, State};
handle_call({list}, _From, State) ->
    io:format("~p list~n", [?MODULE]),

    Torrents = State#state.torrents,

    print_list(Torrents),

    {reply, Torrents, State}.

%% Asynchronous


%% @doc API to start added torrents
handle_cast({start}, State) ->
    io:format("~p starting~n",[?MODULE]),
    {noreply, State};

handle_cast({remove}, State) ->
    io:format("~p remove~n",[?MODULE]),
    {noreply, State};

handle_cast({torrent_srv_add_torrent, From, Metainfo}, State) ->
    % Creating info hash
    {ok, Info} = metainfo:get_value(<<"info">>, Metainfo),
    {ok, Info_encoded} = bencode:encode(Info),
    {ok, Info_hash} = utils:encode_hash(Info_encoded),

    % Preparing torrent tuple
    Torrent = {Info_hash, Metainfo},
    Current_torrents = State#state.torrents,

    Atom_hash = list_to_atom(Info_hash),
    case supervisor:start_child(State#state.torrent_sup,
                                [Atom_hash,
                                 [Info_hash,
                                  Metainfo]
                                ]
                               ) of
        {ok, _Child} ->
            % Adding torrent tuple to the bookkeeping list
            New_state = State#state{torrents = [Torrent|Current_torrents]},

            % Write the updated bookkeeping list to file
            utils:write_term_to_file(?TORRENTS_FILENAME, New_state#state.torrents),

            From ! {reply, ok};
        {error, Reason} ->
            New_state = State,

            case Reason of
                already_present ->
                    From ! {reply, {not_ok, already_present}};
                {already_present, _Child} ->
                    From ! {reply, {not_ok, already_present}};
                Reason ->
                    From ! {reply, {not_ok, unexpected, Reason}}
            end
    end,

    {reply, Info_hash, New_state};
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

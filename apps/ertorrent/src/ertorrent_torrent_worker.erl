%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_worker).

-behaviour(gen_server).

-export([start_link/2,
         shutdown/1,
         start/1,
         start_torrent/1,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-include("ertorrent_log.hrl").

-define(ANNOUNCE_TIME, 30000).
-define(BINARY, ertorrent_binary_utils).
-define(HASH_SRV, ertorrent_hash_srv).
-define(METAINFO, ertorrent_metainfo).
-define(SETTINGS_SRV, ertorrent_settings_srv).
-define(TRACKER, ertorrent_tracker_dispatcher).
-define(TORRENT_SRV, ertorrent_torrent_srv).
-define(UTILS, ertorrent_utils).

% EXECUTION ORDER:
% init
% hash_files_resp
% USER INPUT
% tracker_announce
% add_rx_peer

%% @doc Type to represent the internal state of a torrent worker
%% @initializing == before hashing is completed
%% @inactive == after hashing has been completed and before the user has called
%% start
%% @active == everything is up and running
%% @end

%% @type state_type()
-type state_type() :: initializing | active | inactive.

%% @doc The event type is used to represent the state of the torrent towards
%% the tracker.
%% @end
-type event_type() :: stopped | started | completed.

-export_type([state_type/0,
              event_type/0]).

-record(state, {announce::string(),
                announce_ref::reference(), % Timer reference for tracker announcements
                bitfield::<<>>, % Our bitfield for bookkeeping and peer messages
                compact, % The format of the tracker announcements, 0 = non-compact, 1 = compact
                downloaded::integer(), % Tracker information about how much has been downloaded
                event::event_type(),
                files::tuple(), % Files tuple e.g. {files, multiple, Name, Files_list}
                file_paths::list(),
                info_hash::string(),
                left::integer(), % Tracker information about how much is left to download
                internal_state:: initializing | active | inactive,
                length::integer(), % Total length of the torrent contents
                locations::list(),
                metainfo,
                peers::list(),
                peer_id::string(), % Our unique peer id e.g. ER-1-0-0-<sha1>
                pieces::list(), % Piece hashes from the metainfo
                peers_max::integer(), % The maximum amount of peers this torrent is allowed to have
                peers_cur::integer(), % Current number of active peers
                start_when_ready::boolean(), % TODO use this, if the torrent should start regardless of user input. Should also be used if you want to activate the torrent whenever the torrent is shifting state from initializing to inactive.
                stats_leechers::integer(), % Stats from the tracker that might be of interest
                stats_seeders::integer(), % Stats from the tracker that might be of interest
                tracker_interval::integer(),
                uploaded::integer()}).

% Starting server
start_link(Info_hash_atom, [Info_hash, Metainfo, Port]) when is_atom(Info_hash_atom) ->
    gen_server:start_link({local, Info_hash_atom}, ?MODULE, [Info_hash, Metainfo], []).

% Shutting down the server
shutdown(Name) ->
    gen_server:cast(Name, {shutdown}).

% Changing state of the torrent to started
start(Name) when is_atom(Name) ->
    gen_server:call(Name, {start}).

% Changing state of the torrent to stopped
stop(Name) when is_atom(Name) ->
    io:format("stopping~n"),
    gen_server:call(Name, {stop}).

%% INTERNAL FUNCTIONS

% Time triggered function
tracker_announce_loop(State) ->
    {ok, _Request_id} = ?TRACKER:announce(State#state.announce,
                                          State#state.info_hash,
                                          State#state.peer_id,
                                          State#state.uploaded,
                                          State#state.downloaded,
                                          State#state.left,
                                          atom_to_list(State#state.event),
                                          State#state.compact),

    % Send message to tracker.
    Ref = erlang:send_after(?ANNOUNCE_TIME, self(), {torrent_w_tracker_announce_loop}),

    {ok, Ref}.

start_torrent(State) ->
    % Ensure that the directory structure is alright
    lists:foreach(fun(Path) ->
                      case filelib:ensure_dir(Path) of
                          ok ->
                              ?DEBUG("ensured path: " ++ Path);
                          {error, Reason} ->
                              ?ERROR("failed to create path: " ++ Path)
                      end
                  end, State#state.file_paths),

    % Check file status

    % Compile a list with files that exists on the filesystem. The only time
    % files are expected to not exist is when the torrent is newly added. When
    % a torrent is added the entire set of files should be created.
    % TODO this should probably contain file sizes and should not be part of process_metainfo
    Missing_files = lists:foldl(fun(Filename, Acc) ->
                                    case filelib:is_file(binary_to_list(Filename)) of
                                        false ->
                                            [Filename| Acc]
                                    end
                                end, [], State#state.file_paths),

    % Create an update state record for the tracker announcement
    New_state_event = State#state{event=started},

    % Initial announce
    {ok, Announce_ref} = tracker_announce_loop(New_state_event),

    New_state = New_state_event#state{announce_ref = Announce_ref,
                                      event = started},
    {ok, New_state}.

%%% Callback module
init([Info_hash, Metainfo]) ->
    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    % Replacing reserved characters
    Peer_id_encoded = edoc_lib:escape_uri(Peer_id),

    {ok, Announce_address} = ?METAINFO:get_value(<<"announce">>, Metainfo),
    {ok, Length} = ?METAINFO:get_value(<<"length">>, Metainfo),
    {ok, Piece_length} = ?METAINFO:get_value(<<"piece length">>, Metainfo),

    % Prepare a list of pieces since, the piece section of the metainfo
    % consists of a concatenated binary of all the pieces.
    {ok, Pieces_bin} = ?METAINFO:get_info_value(<<"pieces">>, Metainfo),
    Pieces = ?UTILS:piece_binary_to_list(Pieces_bin),

    Resolved_files = ?METAINFO:resolve_files(Metainfo),

    Location = ?SETTINGS_SRV:get_sync(download_location),

    % TODO investigate support for allocate
    % ensure_file_entries(Files),
    case Resolved_files of
        {files, multiple, Name, Files_list} ->
            Files_reverse = lists:foldl(fun({Path, _}, Acc) ->
                                            Full_path = Location ++ '/' ++ Path,
                                            [Full_path| Acc]
                                        end, [], Files_list),

            % Need to preserve the order
            Filenames = lists:reverse(Files_reverse);
        {files, single, Name, _} ->
            Filenames = [Location ++ '/' ++ Name]
    end,

    % THIS SHOULD BE THE END OF THE INITIALIZATION
    % hash_files is an async call and we should be ready to serve when we
    % receive its response.
    % TODO possible race condition?
    ?HASH_SRV:hash_files(self(), Filenames, Piece_length),

    State = #state{announce = Announce_address,
                   downloaded = 0,
                   event = stopped,
                   files = Resolved_files,
                   info_hash = Info_hash,
                   left = 0,
                   length = Length,
                   metainfo = Metainfo,
                   peer_id = Peer_id_encoded,
                   pieces = Pieces,
                   uploaded = 0},

    ?DEBUG("new torrent " ++ Info_hash),

    {ok, State}.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

%% Synchronous
handle_call({list}, _From, _State) ->
    io:format("~p list~n",[?MODULE]),
    {ok}.

%% User input that should change the torrent state from inactive to active
%% TODO finish me!
handle_cast({start, From}, State) ->

    case State#state.internal_state of
        inactive ->
            _State = start_doing_stuff;
        active ->
            _State = already_doing_stuff;
        initializing ->
            _State = still_initializing_try_again_later;
        _ ->
            ?ERROR("torrent worker in a broken state: " ++
                   atom_to_list(State#state.internal_state))
    end,

    New_state = State,

    {reply, started, New_state};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, _State) ->
    {ok}.

handle_info({torrent_w_tracker_announce_loop}, State) ->
    {ok, Announce_ref} = tracker_announce_loop(State),

    New_state = State#state{announce_ref = Announce_ref},

    {noreply, New_state};

% TODO finish me!
% - parse the peers
handle_info({tracker_announce, Response}, State) ->
    % Using utility function from metainfo since operates on bdecoded
    % structures.
    {ok, Stats_seeders} = ?METAINFO:get_value(<<"complete">>, Response),
    {ok, Stats_leechers} = ?METAINFO:get_value(<<"incomplete">>, Response),
    {ok, Peers} = ?METAINFO:get_value(<<"peers">>, Response),

    % The peers in a tracker response can come in two forms as a dictionary or
    % binary. If the client announced, requesting compact the format will be
    % binary otherwise dictonary. Is it safe to assume that every tracker
    % supports compact since compact wasn't part of the initial version of the
    % torrent protocol?
    case is_binary(Peers) of
        true ->
            {ok, Peer_list} = ?BINARY:parse_peers(Peers);
        false ->
            Peer_list = lists:foldl(fun({Peer_id, Address_bin, Port}, Acc) ->
                                        % TODO the convertion and resolving to
                                        % proper addresses could be moved to
                                        % the peer_w.

                                        % Convert the bdecoded value from binary to string
                                        Address_str = binary_to_list(Address_bin),

                                        % Translate address from a string to tuple.
                                        % NOTE: This will NOT work for hostnames.
                                        case inet:parse_address(Address_str) of
                                            {ok, Address} ->
                                                Address;
                                            % If parse_address() fails, the value is most likely a hostname
                                            {error, einval} ->
                                                % TODO maybe it is relevant to resolve to IPv6 family as well?
                                                % Trying to resolve the hostname to IPv4 family
                                                {ok, Address} = inet:getaddr(Address_str, inet)
                                        end,

                                        [{Address, Port}| Acc]
                                    end, [], Peers)
    end,

    % TODO this should not be done every time we receive a tracker response
    case State#state.peers_cur < State#state.peers_max of
        true ->
            % Calculate this missing number of peers
            Missing_nbr_peers = State#state.peers_max - State#state.peers_cur,

            % Create a list, equal to the amount of missing peers, to activate
            {Peers_activate, Peers_rest} = lists:split(Missing_nbr_peers, Peer_list),

            % Tell the torrent_s to start the peers
            ?TORRENT_SRV ! {torrent_w_add_rx_peers, Peers_activate}
    end,

    % Assume that the maximum amount of peers has been acheived until a message
    % has been received that says otherwise.
    New_state = State#state{peers_cur = State#state.peers_max,
                            peers = Peers_rest,
                            stats_leechers = Stats_leechers,
                            stats_seeders = Stats_seeders},

    {noreply, State};

%% A peer became inactive so it's time to find a replacement
handle_info({torrent_s_peer_inactive, ID}, State) ->
    {noreply, State};

handle_info({torrent_s_read_piece_req, From, Info_hash, Piece_idx}, State) ->
    % TODO lookup offsets for the piece index and determine with file its located in
    {noreply, State};

%% Response form hashing a single piece
%% TODO update the bitfield and update downloaded, left if the piece hashes match
%% - Match the piece hashes
%% - If piece is ok, look up a new one and ask the peer_w for that one.
handle_info({torrent_s_hash_piece_resp, Index, Hash}, State) ->
    {noreply, State};

%% Response from the initial hashing
%% TODO update the values of downloaded, left
handle_info({torrent_s_hash_files_resp, Hashes}, State) ->
    ?DEBUG("recv torrent_s_hash_files_resp"),

    % Construct a list of the to list in the form [{X_hash, Y_hash}, {X_hash1,
    % Y_hash1}]. Now the comparison can be done in one iteration of the ziped
    % list.
    Ziped_piece_hashes = lists:zip(State#state.pieces, Hashes),

    % Constuct a new list over missing and completed pieces.
    % 0 represents a missing piece.
    % 1 represents a completed piece.
    % The new list will be in reverse order.
    Bitfield_list = lists:foldl(fun({X,Y}, Acc) ->
                                    case X =:= Y of
                                        true -> R = 1;
                                        false -> R = 0
                                    end,
                                    [R| Acc]
                                end, [], Ziped_piece_hashes),

    % Reverse to regain correct order
    Bitfield_list_ordered = lists:reverse(Bitfield_list),

    % Convert list to bitfield
    Bitfield = ?BINARY:list_to_bitfield(Bitfield_list_ordered),

    New_state = State#state{bitfield = Bitfield,
                            internal_state = inactive},

    {noreply, New_state}.

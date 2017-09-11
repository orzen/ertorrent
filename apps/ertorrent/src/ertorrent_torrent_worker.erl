%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% Messages:
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_worker).

-behaviour(gen_server).

-export([activate/1,
         deactivate/1,
         shutdown/1,
         start/1,
         start_link/2,
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
-define(FILE_SRV, ertorrent_file_srv).
-define(HASH_SRV, ertorrent_hash_srv).
-define(METAINFO, ertorrent_metainfo).
-define(SETTINGS_SRV, ertorrent_settings_srv).
-define(TRACKER, ertorrent_tracker_dispatcher).
-define(PEER_SRV, ertorrent_peer_srv).
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
                assigned_pieces::list(), % Tuplelist with the assigned piece index and the peer's id.
                bitfield::<<>>, % Our bitfield for bookkeeping and peer messages
                bitfields::list(), % Current peers' bitfield e.g. [{peer's id (not peer_id), bitfield}]
                compact, % The format of the tracker announcements, 0 = non-compact, 1 = compact
                downloaded::integer(), % Tracker information about how much has been downloaded
                event::event_type(),
                files::tuple(), % Files tuple e.g. {files, multiple, Name, Files_list}
                file_paths::list(),
                file_worker::integer(), % The ID for file workers are unique integers
                info_hash::string(),
                left::integer(), % Tracker information about how much is left to download
                length::integer(), % Total length of the torrent contents
                locations::list(),
                metainfo,
                peers::list(),
                peer_id::string(), % Our unique peer id e.g. ER-1-0-0-<sha1>
                pieces::list(), % Piece hashes from the metainfo
                piece_layout::list(), % A translation between piece index and file offset, e.g. [{Piece_idx, File_path, File_offset, Length}]
                peers_max::integer(), % The maximum amount of peers this torrent is allowed to have
                peers_cur::integer(), % Current number of active peers
                state:: initializing | active | inactive,
                start_when_ready::boolean(), % TODO use this, if the torrent should start regardless of user input. Should also be used if you want to activate the torrent whenever the torrent is shifting state from initializing to inactive.
                stats_leechers::integer(), % Stats from the tracker that might be of interest
                stats_seeders::integer(), % Stats from the tracker that might be of interest
                tracker_interval::integer(),
                uploaded::integer()}).

%%% Client API %%%

% @doc Setting a torrent worker to the state active. The term
% activate/deactivate is chosen so that isn't mistaken for start/1 and stop/1
% which are common client APIs in Erlang OTP modules.
% @end
activate(Torrent_worker_id) ->
    gen_server:cast(Torrent_worker_id, {activate}).

% @doc Setting a torrent worker to the state inactive. The term
% activate/deactivate is chosen so that isn't mistaken for start/1 and stop/1
% which are common client APIs in Erlang OTP modules.
% @end
deactivate(Torrent_worker_id) ->
    gen_server:cast(Torrent_worker_id, {deactivate}).

% Starting server
start_link(Info_hash_atom, [Info_hash, Metainfo]) when is_atom(Info_hash_atom) ->
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

% @doc Timer function for when to perform tracker announcements
% @end
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
                              ?ERROR("failed to create path: " ++ Path ++ ", reason: " ++ Reason)
                      end
                  end, State#state.file_paths),

    % Create an update state record for the tracker announcement
    New_state_event = State#state{event=started},

    % Initial announce
    {ok, Announce_ref} = tracker_announce_loop(New_state_event),

    New_state = New_state_event#state{announce_ref = Announce_ref,
                                      event = started},
    {ok, New_state}.

%%% Callback functions
% TODO:
% - Gather every value fetched from the metainfo, re-group and maybe split up
% the contents of this function.
init([Info_hash, Metainfo, Start_when_ready]) ->
    ?DEBUG("starting to initialize torrent worker: \ninfo_hash: " ++ Info_hash
           ++ "\nmetainfo: " ++ Metainfo ++ "\nstart_when_ready: " ++
           Start_when_ready),

    {ok, Announce_address} = ?METAINFO:get_value(<<"announce">>, Metainfo),
    {ok, Length} = ?METAINFO:get_value(<<"length">>, Metainfo),
    {ok, Piece_length} = ?METAINFO:get_value(<<"piece length">>, Metainfo),

    % Prepare a list of pieces since, the piece section of the metainfo
    % consists of a concatenated binary of all the pieces.
    {ok, Pieces_bin} = ?METAINFO:get_info_value(<<"pieces">>, Metainfo),

    Resolved_files = ?METAINFO:resolve_files(Metainfo),

    Pieces = ?UTILS:piece_binary_to_list(Pieces_bin),

    % URI encoded peer id
    {peer_id_uri, Peer_id_encoded} = ?SETTINGS_SRV:get_sync(peer_id_uri),
    {download_location, Location} = ?SETTINGS_SRV:get_sync(download_location),

    % TODO investigate support for allocate
    case Resolved_files of
        {files, multiple, _Name, Files_list} ->
            Files_reverse = lists:foldl(fun({Path, _}, Acc) ->
                                            Full_path = Location ++ '/' ++ Path,
                                            [Full_path| Acc]
                                        end, [], Files_list),

            % Preserve the order
            File_paths = lists:reverse(Files_reverse);
        {files, single, Name, _} ->
            File_paths = [Location ++ '/' ++ Name]
    end,

    % Calculate the tracker statistics
    % TODO
    % - Should the tracker statistics be written to file instead?
    Downloaded = lists:foldl(fun(File_path, Acc) ->
                                     File_size = filelib:file_size(File_path),
                                     Acc + File_size
                             end, 0, File_paths),

    Left = Length - Downloaded,

    Uploaded = 0,

    % Calculate the piece layout over the file structure
    % TODO rename create_file_mapping
    Piece_layout = ?UTILS:create_file_mapping(Resolved_files, Piece_length),

    % THIS SHOULD BE THE END OF THE INITIALIZATION
    % hash_files is an async call and we should be ready to serve when we
    % receive its response.
    % TODO possible race condition?
    ?HASH_SRV:hash_files(self(), File_paths, Piece_length),

    State = #state{announce = Announce_address,
                   downloaded = Downloaded,
                   event = stopped,
                   files = Resolved_files,
                   file_paths = File_paths,
                   info_hash = Info_hash,
                   left = Left,
                   length = Length,
                   metainfo = Metainfo,
                   peer_id = Peer_id_encoded,
                   pieces = Pieces,
                   piece_layout = Piece_layout,
                   start_when_ready = Start_when_ready,
                   uploaded = Uploaded},

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
% @doc API for starting a torrent
% @end
handle_cast({start}, State) ->
    case State#state.state of
        inactive ->
            % TODO
            % - spawn file_w
            % - spawn peer_w

            New_state = start_torrent(State);
        active ->
            New_state = State;
        initializing ->
            New_state = State#state{start_when_ready = true};
        _ ->
            New_state = State,

            ?ERROR("torrent worker in a broken state: " ++
                   atom_to_list(State#state.state))
    end,

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
            F = fun({Peer_id, Address_bin, Port}, Acc) ->
                    % TODO the convertion and resolving to
                    % proper addresses could be moved to
                    % the peer_w.
                    Peer_id_str = binary_to_list(Peer_id),

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

                    [{Address, Port, Peer_id_str}| Acc]
                end,
            Peer_list = lists:foldl(F, [], Peers)
    end,

    % TODO this should not be done every time we receive a tracker response
    case State#state.peers_cur < State#state.peers_max of
        true ->
            % Calculate this missing number of peers
            Missing_nbr_peers = State#state.peers_max - State#state.peers_cur,

            % Create a list, equal to the amount of missing peers, to activate
            {Peers_activate, Peers_rest} = lists:split(Missing_nbr_peers, Peer_list),

            % Tell the torrent_s to start the peers
            ?PEER_SRV ! {torrent_w_add_rx_peers, Peers_activate}
    end,

    % Assume that the maximum amount of peers has been acheived until a message
    % has been received that says otherwise.
    New_state = State#state{peers_cur = State#state.peers_max,
                            peers = Peers_rest,
                            stats_leechers = Stats_leechers,
                            stats_seeders = Stats_seeders},

    {noreply, New_state};

% @doc A peer worker received a peers bitfield. Store it for the piece
% scheduling. Note: The tag does not contain a suffix (res/req) due to being a
% one-way communication.
% @end
handle_info({peer_w_rx_bitfield, ID, Bitfield}, State) ->
    New_bitfields = [{ID, Bitfield}| State#state.bitfields],

    New_state = State#state{bitfields = New_bitfields},
    % TODO
    % - update the algorithm module

    {noreply, New_state};

% @doc A peer worker need to serve the current bitfield to a connecting peer.
% @end
handle_info({peer_w_tx_bitfield_req, ID}, State) ->
    ID ! {torrent_w_tx_bitfield_res, State#state.bitfield},

    {noreply, State};

% @doc A peer worker needs to fill up its work queue. Calculate the next piece
% to request from the peer.
% @end
% TODO implement me
handle_info({peer_w_rx_piece_req, _ID, _Piece_idx, _Piece_data}, State) ->
    % TODO
    % - figure out if this only happens when a peer worker finished a piece
    % - check that we haven't written the same piece already
    {noreply, State};

% @doc A peer has requested a non-cached piece and it have to be read from
% disk. Translate from piece index to file, offset, length and send a request
% to the file worker.
% @end
handle_info({peer_w_tx_piece_req, From, {Piece_index}}, State) ->
    case lists:keyfind(Piece_index, 1, State#state.piece_layout) of
        {Piece_index, File, Offset, Length} ->
            State#state.file_worker ! {torrent_w_read_offset_req, {{From, Piece_index}, File, Offset, Length}};
        false ->
            ?WARNING("failed to lookup file offset for piece index: " ++ Piece_index)
    end,
    {noreply, State};

% @doc The file worker responded to a previous read offset request. Forward the
% piece data to the peer worker that requested it.
% @end
handle_info({file_w_read_offset_res, _From, {_Piece_idx}}, State) ->
    % TODO lookup offsets for the piece index and determine with file its located in
    {noreply, State};

handle_info({file_w_write_offset_res, _From, {_Info_hash, _Piece_idx}}, State) ->
    % TODO lookup offsets for the piece index and determine with file its located in
    {noreply, State};

% @doc A peer worker terminated. Figure out which peer to connect to and fire
% up a new peer worker if it's necessary.
% @end
handle_info({peer_w_terminate, ID, _Current_piece_index}, State) ->
    % Remove the peer_w's bitfield
    New_bitfields = lists:keytake(ID, 1, State#state.bitfields),

    % TODO
    % - trigger an update of the algorithm module

    % Add the assigned piece index to the list again if it wasn't finished

    % Fire up a new peer_w

    New_state = State#state{bitfields = New_bitfields},

    {noreply, New_state};

%% Response form hashing a single piece
%% TODO update the bitfield and update downloaded, left if the piece hashes match
%% - Match the piece hashes
%% - If piece is ok, look up a new one and ask the peer_w for that one.
handle_info({torrent_s_hash_piece_resp, _Index, _Hash}, State) ->
    {noreply, State};

%% Response from the initial hashing
%% TODO update the values of downloaded, left
% @doc Response from the hashing server. This is used during the initial
% hashing of previously downloaded files for the torrent. This is the last step
% of initialization of a torrent worker.
% @end
handle_info({hash_s_hash_files_res, Hashes}, State) ->
    ?DEBUG("recv hash_s_hash_files_res"),

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
                            state = inactive},

    {noreply, New_state}.

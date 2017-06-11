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
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(SERVER, ?MODULE).
-define(TRACKER, ertorrent_tracker_dispatcher).

-type state_type() :: initializing | active | inactive.

-type event_type() :: stopped | started | completed.

-export_type([state_type/0,
              event_type/0]).

-record(torrent, {announce::string(),
                  files::list()}).

-record(tracker, {downloaded::integer(),
                  left::integer(),
                  uploaded::integer()}).

-record(state, {announce::string(),
                bitfield::list(),
                downloaded::integer(),
                event::event_type(),
                files::list(),
                info_hash::string(),
                internal_state::state_type(),
                left::integer(),
                length::integer(),
                metainfo,
                peers::list(),
                peer_id::string(),
                port::integer(),
                tracker_interval::integer(),
                uploaded::integer()}).

% Starting server
start_link(Name, [Info_hash, Metainfo, Port]) ->
    gen_server:start_link({local, Name}, ?MODULE, [Info_hash, Metainfo, Port], []).

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

initialize_torrent(Metainfo, State) ->
    {ok, Announce_address} = metainfo:get_value(<<"announce">>, Metainfo),
    {ok, Piece_length} = metadinfo:get_value(<<"piece length">>, Metainfo),

    Resolved_files = metainfo:resolve_files(Metainfo),

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

    [{announce, Announce_address},
     {piece_length, Piece_length},
     Resolved_files].


% Time triggered function
announce_trigger(State) ->
    {ok, Request_id} = ?TRACKER:announce(State#state.announce,
                                         State#state.info_hash,
                                         State#state.peer_id,
                                         State#state.port,
                                         State#state.uploaded,
                                         State#state.downloaded,
                                         State#state.left,
                                         atom_to_list(State#state.event),
                                         State#state.compact),

    % Send message to tracker.
    erlang:send_after(?ANNOUNCE_TIME, self(), {announce}),
    ok.

%%% Callback module
init([Info_hash, Metainfo, Port]) ->
    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    % Replacing reserved characters
    Peer_id_encoded = edoc_lib:escape_uri(Peer_id),

    [{announce, Announce_address},
     {piece_length, Piece_length},
     Resolved_files] = initialize_torrent(Metainfo),

    State = #state{metainfo=Metainfo,
                   announce=Announce_address,
                   info_hash=Info_hash,
                   peer_id=Peer_id_encoded,
                   port=Port,
                   length=Length,
                   uploaded=0,
                   downloaded=0,
                   left=0,
                   event="stopped"},

    ?DEBUG("new torrent " ++ Info_hash),

    {ok, State}.

%% Synchronous
handle_call({start}, _From, State) ->
    New_state = State#state{event=started},

    % Check file status

    % Compile a list with files that exists on the filesystem. The only time
    % files are exoected to not exist is when the torrent is newly added. When
    % a torrent is added the entire set of files should be created.
    % TODO this should probably contain file sizes and should not be part of process_metainfo
    File_list = lists:foldl(fun(Filename, Acc) ->
                                case filelib:is_file(binary_to_list(Filename)) of
                                    true ->
                                        [{Filename, true}|Acc];
                                    false ->
                                        [{Filename, false}|Acc]
                                end,
                            end, [], Download_files).

    % Initial announce
    ok = announce(New_state),


    {reply, started, New_state};
handle_call({list}, _From, _State) ->
    io:format("~p list~n",[?MODULE]),
    {ok}.

%% Asynchronous
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, _State) ->
    {ok}.

handle_info({tracker, Response}, State) ->
    {ok}.
handle_info({torrent_s_read_piece_req, From, Info_hash, Piece_idx}, State) ->
    % TODO lookup offsets for the piece index and determine with file its located in

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.


%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(TRACKER, ertorrent_tracker_dispatcher).

-include("ertorrent_log.hrl").

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

-type state_type() :: initializing | active | inactive.

-type event_type() :: stopped | started | completed.

-export_type([state_type/0, event_type/0]).

-record(state, {internal_state::state_type(),
                tracker_interval::integer(),
                peers::list(),
                metainfo::string(),
                announce::string(),
                info_hash::string(),
                peer_id::string(),
                port::integer(),
                length::integer(),
                uploaded::integer(),
                downloaded::integer(),
                left::integer(),
                event::event_type(),
                bitfield::list()}).

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
    {ok, Announce_address} = metainfo:get_value(<<"announce">>, Metainfo),
    {ok, Info} = metainfo:get_value(<<"info">>, Metainfo),
    {ok, Name} = metainfo:get_value(<<"name">>, Info),
    {ok, Length} = metainfo:get_value(<<"length">>, Info),

    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    % Replacing reserved characters
    Peer_id_encoded = edoc_lib:escape_uri(Peer_id),

    % TODO check with the file handler if something already, been downloaded
    case filelib:is_file(binary_to_list(Name)) of
        true ->
            ok;
        false ->
            ok
    end,

    State = #state{metainfo=Metainfo, announce=Announce_address,
                   info_hash=Info_hash, peer_id=Peer_id_encoded, port=Port,
                   length=Length, uploaded=0, downloaded=0, left=0,
                   event="stopped"},

    ?DEBUG("new torrent " ++ Info_hash),

    {ok, State}.

%% Synchronous
handle_call({start}, _From, State) ->
    New_state = State#state{event=started},

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

hanele_info({tracker, Response}, State) ->
    {ok}.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.


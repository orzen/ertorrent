%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_worker).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start/1,
         stop/1,
         terminate/1]).

-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type event_type()::'stopped' | 'started' | 'completed' | ''.

-record(state, {metainfo::string(),
                announce::string(),
                info_hash::string(),
                peer_id::string(),
                port::integer(),
                length::integer(),
                uploaded::integer(),
                downloaded::integer(),
                left::integer(),
                event::event_type()}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% Standard client API
start_link(Name, [Info_hash, Metainfo, Port]) ->
    gen_server:start_link({local, Name}, ?MODULE, [Info_hash, Metainfo, Port], []).

terminate(Name) ->
    io:format("stopping~n"),
    gen_server:cast(Name, stop).

% Changing state of the torrent to started
start(Name) when is_atom(Name) ->
    gen_server:call(Name, {start}).

% Changing state of the torrent to stopped
stop(Name) when is_atom(Name) ->
    gen_server:call(Name, {stop}).

%%% Callback module
init([Info_hash, Metainfo, Port]) ->
    {ok, Announce_address} = metainfo:get_value(<<"announce">>, Metainfo),
    {ok, Info} = metainfo:get_value(<<"info">>, Metainfo),
    {ok, Length} = metainfo:get_value(<<"length">>, Info),

    Peer_id = string:concat("ET-0-0-1", string:chars($ , 12)),
    % Replacing reserved characters
    Peer_id_encoded = edoc_lib:escape_uri(Peer_id),
    error_logger:info_msg("~p: spawned ~p~n", [?MODULE, Info_hash]),
    {ok, #state{metainfo=Metainfo, announce=Announce_address,
                info_hash=Info_hash, peer_id=Peer_id_encoded, port=Port,
                length=Length, uploaded=0, downloaded=0, left=0,
                event="stopped"}}.

%% Synchronous
handle_call({start}, _From, State) ->
    Request = ertorrent_tracker_protocol:new_request(State#state.announce,
                                                     State#state.info_hash,
                                                     State#state.peer_id,
                                                     State#state.port,
                                                     State#state.uploaded,
                                                     State#state.downloaded,
                                                     State#state.left,
                                                     State#state.event,
                                                     1),
    Resp = ertorrent_tracker_protocol:send(Request),
    {reply, Resp, State};
handle_call({list}, _From, _State) ->
    io:format("~p list~n",[?MODULE]),
    {ok}.

%% Asynchronous
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Request, _State) ->
    {ok}.

%% Do work here
handle_info(_Info, _State) ->
    ok.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.


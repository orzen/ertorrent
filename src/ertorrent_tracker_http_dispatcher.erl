%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_tracker_dispatcher).

-behaviour(gen_server).

-include("ertorrent_debug.hrl").

-define(TRACKER_REQUEST, ertorrent_tracker_request).

-export([announce/9,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% mapping, maps HTTP request ID with dispatch request.
-record(state, {mapping::list()}).

announce(Address, Info_hash, Peer_id, Port, Uploaded, Downloaded, Left, Event,
         Compact) ->
    Request = ?TRACKER_REQUEST:new_request(Address,
                                           Info_hash,
                                           Peer_id,
                                           Port,
                                           Uploaded,
                                           Downloaded,
                                           Left,
                                           Event,
                                           Compact).

    gen_server:call(?MODULE, {announce, Request}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    inets:start(),
    {ok, #state{}}.

handle_call({announce, Request}, From, State) ->
    {ok, Request_id} = httpc:request(get, {Request, [{"Accept", "text/plain"}]},
                                    [], [{sync, false}, {headers_as_is, true}]),

    Mapping = [{Request_od, From} | State#state.mapping],

    {reply, {ok, Request_id}, State#state{mapping=Mapping}}.

handle_cast(_Req, State) ->
    {noreply, State}.

% Handle the tracker response
handle_info({http, {Request_id, Response}}, State) ->
    case lists:keyfind(Request_id, 1, State#state.mapping) of
        {Request_id, From} ->
            ?DEBUG("Received response " ++ Response),
            ?DEBUG("Passing response to torrent: " ++ From),

            % Updateing the mapping
            Mapping = lists:delete({Request_id, From}, State#state.mapping),

            % Decode and send response to the torrent process
            Decoded_response = bencode:decode(Response),
            From ! {tracker, Decoded_response},

            New_state = State#state{mapping=Mapping};
        false ->
            % This is required to provide a new state in the previous clause.
            New_state = State,
            error_logger:warning_report("Received a response for an untracked RequestId")
    end,

    {noreply, New_state}.

terminate(shutdown, _State) ->
    inets:stop().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

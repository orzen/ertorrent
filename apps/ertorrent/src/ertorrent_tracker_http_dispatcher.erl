%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_tracker_http_dispatcher).

-behaviour(gen_server).

-include("ertorrent_log.hrl").

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
-record(state, {requests::list()}).

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
                                           Compact),

    gen_server:cast(?MODULE, {announce, self(), Request}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    inets:start(),
    {ok, #state{}}.

handle_call(Req, From, State) ->
    ?WARNING("unhandled call: " ++ Req ++ " from: " ++ From),
    {noreply, State}.

handle_cast({announce, From, Request}, State) ->
    {ok, Request_id} = httpc:request(get, {Request, [{"Accept", "text/plain"}]},
                                    [], [{sync, false}, {headers_as_is, true}]),

    Requests = [{Request_id, From} | State#state.requests],

    {noreply, State#state{requests=Requests}};

handle_cast(Req, State) ->
    ?WARNING("unhandled cast: " ++ Req),
    {noreply, State}.

% Handle the tracker response
handle_info({http, {Request_id, Response}}, State) ->
    case lists:keyfind(Request_id, 1, State#state.requests) of
        {Request_id, From} ->
            ?DEBUG("Received response " ++ Response),
            ?DEBUG("Passing response to torrent: " ++ From),

            % Updating the mapping
            Requests = lists:delete({Request_id, From}, State#state.requests),

            % Decode and send response to the torrent process
            Decoded_response = bencode:decode(Response),
            From ! {tracker_http_dispatcher_res, Decoded_response},

            New_state = State#state{requests=Requests};
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

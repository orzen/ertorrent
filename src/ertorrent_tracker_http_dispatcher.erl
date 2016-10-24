%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_tracker_http_dispatcher).

-behaviour(gen_server).

-export([announce/9,
         announce2/9,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-define(BENCODE, ertorrent_bencode).
-define(TRACKER_REQUEST, ertorrent_tracker_request).
-define(UTILS, ertorrent_utils).

% mapping, maps HTTP request ID with dispatch request.
-record(state, {requests::list()}).

announce2(Address_bin, Info_hash_bin, Peer_id, Port, Uploaded, Downloaded,
          Left, Event, Compact) ->
    % Normalize arguments
    Info_hash_enc = hackney_url:urlencode(Info_hash_bin, [noplus, upper]),

    lager:debug("PEER ID '~p'", [length(Peer_id)]),
    Peer_id_bin = list_to_binary(Peer_id),
    Peer_id_enc = hackney_url:urlencode(Peer_id_bin, [noplus, upper]),

    Port_bin = list_to_binary(integer_to_list(Port)),
    Uploaded_bin = list_to_binary(integer_to_list(Uploaded)),
    Downloaded_bin = list_to_binary(integer_to_list(Downloaded)),
    Left_bin = list_to_binary(integer_to_list(Left)),

    Event_bin = list_to_binary(Event),

    Compact_bin = list_to_binary(integer_to_list(Compact)),

    Request = ?TRACKER_REQUEST:new_bin_request(Address_bin,
                                               Info_hash_enc,
                                               Peer_id_enc,
                                               Port_bin,
                                               Uploaded_bin,
                                               Downloaded_bin,
                                               Left_bin,
                                               Event_bin,
                                               Compact_bin),
    lager:debug("~p: ~p: prepared request '~s'", [?MODULE, ?FUNCTION_NAME, Request]),

    case hackney:request(get, Request, [], <<>>, []) of
        {ok, 200, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            ?BENCODE:decode(Body);
        {ok, Status_code, _Headers, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            lager:warning("~p: ~p: bad HTTP response(~p): '~p'", [?MODULE,
                                                                  ?FUNCTION_NAME,
                                                                  Status_code,
                                                                  Body]),
            error;
        {error, Reason} ->
            lager:warning("failed to announce: '~p'", [Reason]),
            error
    end.

% TODO make use of compact
announce(Address, Info_hash, Peer_id, Port, Uploaded, Downloaded, Left, Event, _Compact) ->
    % Normalize arguments
    Address_str = binary_to_list(Address),
    Info_hash_enc = ?UTILS:percent_encode(Info_hash),

    Request = ?TRACKER_REQUEST:new_request(Address_str,
                                           Info_hash_enc,
                                           Peer_id,
                                           Port,
                                           Uploaded,
                                           Downloaded,
                                           Left,
                                           Event,
                                           0),

    gen_server:call(?MODULE, {announce, Request}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    inets:start(),
    {ok, #state{}, hibernate}.

handle_call({announce, Request}, From, State) ->
    {ok, Request_id} = httpc:request(get, {Request,
                                           [{"Connection", "close"},
                                            {"Accept", "*/*"}]},
                                           [],
                                           [{sync, false},
                                            {header_as_is, true},
                                            {receiver, self()}]),

    Requests = [{Request_id, From} | State#state.requests],

    {reply, {ok, Request_id}, State#state{requests=Requests}, hibernate};

handle_call(Req, From, State) ->
    lager:warning("unhandled call: '~p', from: '~p'", [Req, From]),
    {noreply, State}.

handle_cast(Req, State) ->
    lager:warning("unhandled cast: '~p'", [Req]),
    {noreply, State}.

% Handle the tracker response
handle_info({http, {Request_id, Response}}, State) ->
    case lists:keyfind(Request_id, 1, State#state.requests) of
        {Request_id, From} ->
            lager:warning("Received response: '~p'~nForwarding response to torrent worker: '~p'",
                        [Response, From]),

            % Updating the mapping
            Requests = lists:delete({Request_id, From}, State#state.requests),

            % Decode and send response to the torrent process
            {ok, Decoded_response} = ?BENCODE:decode(Response),
            From ! {tracker_http_dispatcher_res, Decoded_response},

            New_state = State#state{requests=Requests};
        false ->
            % This is required to provide a new state in the previous clause.
            New_state = State,
            lager:warning("Received a response from an untracked request: '~p'", [Request_id])
    end,

    {noreply, New_state, hibernate}.

terminate(normal, _State) ->
    inets:stop();
terminate(shutdown, _State) ->
    inets:stop().

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

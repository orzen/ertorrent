-module(ertorrent_settings_srv).

-behaviour(gen_server).

-export([get_sync/1,
         start_link/0,
         shutdown/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-record(state, {settings::list()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

shutdown() ->
    gen_server:cast(?MODULE, {shutdown}).

get_sync(Setting) when is_atom(Setting) ->
    gen_server:call(?MODULE, {settings_srv_get, Setting}).

init(_Args) ->
    % TODO
    % Add support for configuration files
    % Look for configuration file in /etc
    % Look for configuration file in ~/.ertorrent

    Padding = string:chars($ , 12),
    Peer_id = lists:concat(["ET-0-0-1", Padding]),
    % Replacing reserved characters
    Peer_id_encoded = http_uri:encode(Peer_id),

    Settings = [
        {download_location,"~/ertorrent/downloads"},
        {peer_id_str,Peer_id},
        {peer_id_uri,Peer_id_encoded},
        {peer_listen_port,35400}
    ],

    State = #state{settings=Settings},

    {ok, State}.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

handle_call({settings_srv_get, Setting}, _From, State) ->
    Result = lists:keyfind(Setting, 1, State#state.settings),

    {reply, Result, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

-module(ertorrent_settings_srv).

-behaviour(gen_server).

-export([get_sync/1,
         start_link/1,
         shutdown/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-include("ertorrent_log.hrl").

-record(state, {download_location::string()}).

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).

shutdown() ->
    gen_server:cast(?MODULE, {shutdown}).

get_sync(Setting) when is_atom(Setting) ->
    gen_server:call(?MODULE, {settings_srv_get, Setting}).

init([Filename]) ->
    % TODO
    % Add support for configuration files
    % Look for configuration file in /etc
    % Look for configuration file in ~/.ertorrent

    State = #state{download_location="~/ertorrent/downloads"},

    {ok, State}.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

handle_call({settings_srv_get, download_location}, From, State) ->
    {reply, State#state.download_location, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

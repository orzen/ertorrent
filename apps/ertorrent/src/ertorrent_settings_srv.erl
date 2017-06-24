-module(ertorrent_settings_srv).

-behaviour(gen_server).

-export([start_link/2,
         shutdown/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ertorrent_log.hrl").

-record(settings, {download_location::string()}).

-record(state, {settings::record()}).

start_link([Filename]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).

shutdown() ->
    gen_server:cast(?MODULE, {shutdown}).

get_sync(Setting) when is_atom(Settings) ->
    gen_server:call(?MODULE, {settings_srv_get, Settings}).

init() ->
    % TODO
    % Add support for configuration files
    % Look for configuration file in /etc
    % Look for configuration file in ~/.ertorrent

    Default_settings = #settings{download_location="~/ertorrent/downloads"},

    State = #state{settings=Default_settings},

    {ok, State}.

handle_call({settings_srv_get, download_location}, From, State) ->
    Settings = State#state.settings,
    {reply, Settings#setting.download_location, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

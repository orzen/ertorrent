-module(ertorrent_settings_srv).

-behaviour(gen_server).

-export([get_conf/1,
         start_link/0,
         shutdown/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-include("ertorrent_log.hrl").

-record(state, {configs :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

shutdown() ->
    gen_server:cast(?MODULE, {shutdown}).

get_conf(Conf) when is_atom(Conf) ->
    gen_server:call(?MODULE, {get_conf, Conf}).

init(_Args) ->
    % TODO
    % Add support for configuration files
    % Look for configuration file in /etc
    % Look for configuration file in ~/.ertorrent

    % Generate a unique id. 12 bytes together with the six characters from the
    % prefix will result in total 20 characters when encoded.
    Id_bin = crypto:strong_rand_bytes(14),
    Id_str = binary_to_list(Id_bin),
    Peer_id = unicode:characters_to_list(["ET001-", Id_str]),
    Peer_id2 = list_to_binary(Peer_id),

    Configs = #{
        block_length => math:pow(2, 14), % 16kb
        download_location => "~/ertorrent/downloads",
        peer_id => Peer_id2,
        peer_listen_port => 35400
    },

    State = #state{configs = Configs},

    {ok, State}.

terminate(Reason, _State) ->
    io:format("~p: going down, with reason '~p'~n", [?MODULE, Reason]),
    ok.

handle_call({get_conf, Conf}, _From, State = #state{configs = Configs}) ->
    Result =  maps:find(Conf, Configs),
    {reply, Result, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

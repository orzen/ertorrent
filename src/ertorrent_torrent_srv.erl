%%%-------------------------------------------------------------------
%%% @todo read up on edoc
%%% @doc torrent_gen, this is the interface to interact with the
%%% torrents.
%%% @end
%%%-------------------------------------------------------------------

-module(ertorrent_torrent_srv).

-behaviour(gen_server).

-include("ertorrent_log.hrl").

-define(SERVER, ?MODULE).
-define(TORRENTS_FILENAME, "TEST_FILE").

-export([start_link/1,
         stop/0,
         add/1,
         list/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {port, supervisor, torrents=[]}).

add(Metainfo) ->
    gen_server:call(?MODULE, {add, Metainfo}).

list(Pid) ->
    gen_server:call(Pid, {list}).

print_list([]) ->
    true;
print_list([H|T]) ->
    {Hash, _} = H,
    io:format("~p~n", [Hash]),
    print_list(T).

%%% Standard client API
% Args = [Port]
start_link([Port]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    io:format("Stopping: ~p...~n", [?MODULE]),
    gen_server:cast(?MODULE, stop).

%%% Callback module
init([Port]) ->
    case filelib:is_file(?TORRENTS_FILENAME) of
        true ->
            {ok, Torrents} = utils:read_term_from_file(?TORRENTS_FILENAME),
            lists:foreach(
                fun({Info_hash, Metainfo}) ->
                    supervisor:start_child(ertorrent_torrent_sup,
                                           [list_to_atom(Info_hash),
                                            [Info_hash, Metainfo,
                                             Port]])
                end,
                Torrents);
        false ->
            Torrents = []
    end,
    {ok, #state{port=Port, torrents=Torrents}}.

%% Synchronous
handle_call({start}, _From, _State) ->
    io:format("~p starting~n",[?MODULE]),
    {ok};
handle_call({add, Metainfo}, _From, State) ->
    Name = metainfo:get_info_value(<<"name">>, Metainfo),

    % Creating info hash
    {ok, Info} = metainfo:get_value(<<"info">>, Metainfo),
    {ok, Info_encoded} = bencode:encode(Info),
    {ok, Info_hash} = utils:encode_hash(Info_encoded),

    % Adding torrent tuple to the bookkeeping list
    Torrent = {Info_hash, Metainfo},
    Current_torrents = State#state.torrents,
    New_state = State#state{torrents = [Torrent|Current_torrents]},

    % Write the updated bookkeeping list to file
    utils:write_term_to_file(?TORRENTS_FILENAME, New_state#state.torrents),

    Atom_hash = list_to_atom(Info_hash),
    Ret = supervisor:start_child(ertorrent_torrent_sup,
                                 [Atom_hash,
                                  [Info_hash,
                                   Metainfo,
                                   New_state#state.port
                                  ]
                                 ]
                                ),

    {reply, Info_hash, New_state};

handle_call({remove}, _From, _State) ->
    io:format("~p remove~n",[?MODULE]),
    {ok};
handle_call({list}, _From, State) ->
    io:format("~p list~n",[?MODULE]),

    Torrents = State#state.torrents,

    print_list(Torrents),

    {reply, ok, State}.

%% Asynchronous
handle_cast(stop, State) ->
    {stop, normal, State}.

%% Do work here
handle_info({'EXIT', _ParentPid, shutdown}, State) ->
    {stop, shutdown, State};
handle_info(_Info, _State) ->
    ok.

terminate(Reason, _State) ->
    io:format("~p: going down, Reason: ~p~n", [?MODULE, Reason]),
    error_logger:info_msg("~p: terminating, reason: ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.


-ifdef(TEST).

dev_test() ->
    erlang:display("Running developer test"),
    {ok, Meta} = metainfo:read_file("../src/debian-8.3.0-amd64-netinst.iso.torrent"),
    io:format("meta: ~p~n", [Meta]),
    {ok, Pid} = torrent_gen:start_link(),
    {ok, Pid2} = torrent_gen:start_link(),
    erlang:display(Pid),
    erlang:display(Pid2),
    Ret = torrent_gen:stop(Pid),
    torrent_gen:stop(Pid2),
    erlang:display(Ret),
    ?assert(true).

-endif.

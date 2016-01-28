-module(torrent_gen).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket})

%%% Client API
start() ->
    gen_server:start_link().

stop(Pid) ->
    gen_server:call(Pid, terminate).

status() ->
    {ok}.

%%% gen_server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([Socket, Metainfo]) ->
    {ok, #state{}}.

%% Synchronous
handle_call(start, From, State) ->
    {ok}.
handle_call(stop, From, State) ->
    {ok}.
handle_call(Request, From, State) ->
    {ok}.

%% Asynchronous
handle_cast(Message, State) ->
    {ok}.

%% Do work here
handle_info(Info, State) ->
    #state{socket=Socket} = State,
    case gen_tcp:recv(Socket, 0) of
        {error, closed} ->
            {ok, list_to_binary(Bs)}
        {ok, Packet} ->
            io:format("recv: ~w\n", [Packet]).
    end

terminate(Reason, State) ->
    {ok}.

code_change(One, Two, Three) ->
    {ok}.

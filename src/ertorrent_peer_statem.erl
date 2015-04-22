-module(ertorrent_peer_statem).
-behaviour(gen_statem).

-export([
         choke/1,
         unchoke/1,
         feed/2,
         start_link/2,
         stop/1
        ]).

% gen_statem callback functions
-export([
         init/1,
         terminate/3,
         callback_mode/0
        ]).

% State name callback functions
-export([
         idle/3,
         run/3
        ]).

-record(data, {
               id::atom(),
               queue::list(),
               peer_ref::atom()
              }).

-define(PEER_SRV, ertorrent_peer_srv).
-define(PEER_W, ertorrent_peer_worker).

-type data()::#data{}.
-type event_content()::choke | unchoke | stop | {feed, Queue::list()}.
-type idle_result()::{keep_state, Data::data()} |
        {next_state, run, Data::data()} |
        {next_state, run, Data::data(), [{next_event, cast, unchoke}]} |
        {repeat_state, Data::data()}.
-type run_result()::{keep_state, Data::data()} |
        {next_state, idle, Data::data()} |
        {next_state, idle, Data::data(), [{next_event, cast, unchoke}]}.

%%% Module API

-spec choke(Self_ref::atom()) -> ok.
choke(Self_ref) ->
    gen_statem:cast(Self_ref, choke).

-spec unchoke(Self_ref::atom()) -> ok.
unchoke(Self_ref) ->
    gen_statem:cast(Self_ref, unchoke).

-spec feed(Self_ref::atom(), Queue::list()) -> ok.
feed(Self_ref, Queue) when is_list(Queue) ->
    gen_statem:cast(Self_ref, {feed, Queue}).

-spec start_link(Self_ref::atom(), Peer_ref::atom()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Self_ref, Peer_ref) ->
    gen_statem:start_link({local, Self_ref}, ?MODULE, [Self_ref, Peer_ref], []).

-spec stop(Self_ref::atom()) -> ok.
stop(Self_ref) ->
    gen_statem:cast(Self_ref, stop).

dispatch([]) ->
    ok;
dispatch([{Create_request, Socket, Request_details}| Rest]) ->
    lager:debug("~p: dispatching '~p'~n", [?FUNCTION_NAME, Request_details]),

    try Create_request(Request_details) of
        Request ->
            case gen_tcp:send(Socket, Request) of
                ok ->
                    dispatch(Rest);
                {error, Reason} ->
                    lager:warning("~p: ~p: failed to send '~p', reason '~p'",
                                  [?MODULE, ?FUNCTION_NAME, Request, Reason]),
                    error
            end
    catch
        Class:Reason -> lager:error("~p:~p: class '~p', reason '~p'", [?MODULE, ?FUNCTION_NAME, Class, Reason])
    end.

%%% Behaviour callback functions

-spec init(Args::list()) -> {ok, idle, data()}.
init([Self_ref, Peer_ref]) ->
    {ok, idle, #data{id = Self_ref,
                     queue = [],
                     peer_ref = Peer_ref}}.

terminate(Reason, _State, Data) ->
    % TODO return the remaining queue, if there is one, when terminating
    lager:debug("~p:~p: terminating '~p'", [?MODULE, ?FUNCTION_NAME, Reason]),

    ?PEER_SRV:statem_terminated(Data#data.id),

    ok.

callback_mode() ->
    state_functions.

%%% State name callback functions

-spec idle(cast, Old_state::event_content(), Data::data()) -> idle_result().
idle(cast, choke, Data) ->
    lager:debug("~p: ~p: choke", [?MODULE, ?FUNCTION_NAME]),
    {keep_state, Data};
idle(cast, unchoke, Data) ->
    lager:debug("~p: ~p: unchoke", [?MODULE, ?FUNCTION_NAME]),
    case Data#data.queue == [] of
        false ->
            {next_state, run, Data, [{next_event, cast, unchoke}]};
        true ->
            {keep_state, Data}
    end;
idle(cast, {feed, Queue}, Data) ->
    lager:debug("~p: ~p: feed '~p'", [?MODULE, ?FUNCTION_NAME, Queue]),
    New_queue = lists:merge(Queue, Data#data.queue),
    % Refilling the dispatch queue and continue to idle.
    New_data = Data#data{queue = New_queue},
    {repeat_state, New_data};
idle(Event_type, Event_content, Data) ->
    handle_event(Event_type, Event_content, Data).

-spec run(cast, Old_state::event_content(), Data::data()) -> run_result().
run(cast, choke, Data) ->
    lager:debug("~p: ~p: choke", [?MODULE, ?FUNCTION_NAME]),
    {next_state, idle, Data};
run(cast, unchoke, Data) ->
    lager:debug("~p: ~p: unchoke", [?MODULE, ?FUNCTION_NAME]),

    case Data#data.queue == [] of
        false ->
            case dispatch(Data#data.queue) of
                ok ->
                    ?PEER_W:request_blocks(Data#data.peer_ref),
                    {keep_state, Data#data{queue = []}};
                error ->
                    {stop, normal, Data}
            end;
        true ->
            {keep_state, Data}
    end;
run(cast, {feed, Queue}, Data) ->
    lager:debug("~p: ~p: feed '~p'", [?MODULE, ?FUNCTION_NAME, Queue]),

    New_queue = lists:merge(Queue, Data#data.queue),
    New_data = Data#data{queue = New_queue},

    % Since current state is 'run' it is safe to assume that the previous event
    % is unchoke. By changing state to idle and emit another event another
    % dispatch should be triggered.
    {next_state, idle, New_data, [{next_event, cast, unchoke}]};
run(Event_type, Event_content, Data) ->
    handle_event(Event_type, Event_content, Data).

%%% State common events

handle_event(cast, stop, Data) ->
    lager:debug("~p: ~p: stop", [?MODULE, ?FUNCTION_NAME]),
    {stop, normal, Data};

% Catching everything else
handle_event(EventType, EventContent, Data) ->
    lager:error("~p:~p: UNHANDLED TYPE '~p' CONTENT '~p'",
                [?MODULE, ?FUNCTION_NAME, EventType, EventContent]),
    {keep_state, Data}.

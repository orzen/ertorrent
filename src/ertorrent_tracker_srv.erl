-module(ertorrent_tracker_srv).

-export([
         subscribe/1,
         start_link/0,
         unsubscribe/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-record(state, {
          trackers = #{} :: map()
         }).

-include("ertorrent_modules.hrl").
-include("ertorrent_tracker.hrl").
-include("ertorrent_internal.hrl").

-define(SERVER, {local, ?MODULE}).

%% API %%

subscribe(Subscribe) ->
    gen_server:cast(?MODULE, Subscribe).

unsubscribe(Unsubscribe) ->
    gen_server:cast(?MODULE, Unsubscribe).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% CALLBACKS %%

init(_Args) ->
    lager:info("~s(~w): started", [?MODULE, self()]),
    {ok, #state{}, hibernate}.

handle_call(Request, From, State) ->
    lager:error("~s(~w): unhandled call '~w' '~s'",
                [?MODULE, self(), From, Request]),
    {stop, unhandled_call, State}.

handle_cast(#'tracker.srv.subscribe'{uri = URI,
                                     reply = Client_reply},
            State = #state{trackers = Trackers}) ->
    lager:info("~s(~w): connect", [?MODULE, self()]),

    {Tracker, New_trackers} = prepare_tracker(Client_reply, URI, Trackers),

    Own_reply = l_reply(),
    Tracker#tracker.id ! #'tracker.worker.subscribe'{client_pid = Client_reply#reply.id,
                                                     reply = Own_reply},

    {noreply, State#state{trackers = New_trackers}, hibernate};
handle_cast(#'tracker.srv.unsubscribe'{ tracker_worker_pid = ID, reply = Reply },
            State = #state{trackers = Trackers}) ->
    #{ID := Tracker} = Trackers,
    {Refs, Trackers2} = unref(Reply, Tracker, Trackers),

    Trackers3 = case Refs =:= 0 of
        false -> Trackers2;
        true -> remove_tracker(Tracker, Trackers2)
    end,

    Reply ! #'tracker.srv.unsubscribed'{ tracker_worker_pid = ID },

    {noreply, State#state{trackers = Trackers3}, hibernate};
handle_cast(Request, State) ->
    lager:error("~s(~w): unhandled cast '~p'", [?MODULE, self(), Request]),
    {stop, unhandled_cast, State}.

handle_info(#'tracker.worker.subscribed'{client_pid = Client,
                                         tracker_worker_pid = Tracker}, State) ->
    Client ! #'tracker.srv.subscribed'{tracker_worker_pid = Tracker},
    {noreply, State, hibernate};
handle_info(Msg, State) ->
    lager:error("~s(~p): unhandled info '~p'", [?MODULE, self(), Msg]),
    {stop, unhandled_info, State}.

%% Internal functions %%

prepare_tracker(Reply, URI, Trackers) ->
    P = uri_string:parse(URI),
    #{scheme := Scheme, host := Host, port := Port} = P,
    Proto = list_to_atom(Scheme),

    ID = list_to_atom(URI),

    % Check if tracker exists
    try maps:get(ID, Trackers) of
        T ->
            {_Refs, Trackers2} = ref(Reply, T, Trackers),
            {T, Trackers2}
    catch
        % badkey means that it's a new tracker
        %{badkey, ID} ->
        _:_ ->
            T = #tracker{id = ID,
                         proto = Proto,
                         host = Host,
                         port = Port,
                         refs = 1},
            add_tracker(T, Trackers)
    end.

update_clients(Clients, Tracker = #tracker{id = ID}, Trackers) ->
    Trackers2 = Trackers#{ID => Tracker#tracker{clients = Clients}},
    {length(Clients), Trackers2}.

ref(Client, Tracker = #tracker{clients = Clients}, Trackers) ->
    Clients2 = Clients#{Client#reply.id => Client},
    update_clients(Clients2, Tracker, Trackers).

unref(Client_id, Tracker = #tracker{clients = Clients}, Trackers) ->
    Clients2 = maps:without([Client_id], Clients),
    update_clients(Clients2, Tracker, Trackers).

add_tracker(Tracker = #tracker{id = ID}, Trackers) ->
    {ok, _Pid} = ?TRACKER_SUP:start_child(Tracker),
    Trackers2 = Trackers#{ID => Tracker},
    {Tracker, Trackers2}.

remove_tracker(Tracker = #tracker{id = ID}, Trackers) ->
    ok = ?TRACKER_SUP:terminate_child(Tracker),
    maps:remove(ID, Trackers).

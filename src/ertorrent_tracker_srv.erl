-module(ertorrent_tracker_srv).

-export([
         connect/1,
         disconnect/1
        ]).

-export([
         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
          trackers = #{} ::map()
         }).

-include("ertorrent_modules.hrl").
-include("ertorrent_tracker.hrl").

-define(SERVER, {local, ?MODULE}).

%% API %%

connect(Connect) ->
    lager:info("~s: connect '~p'", [?MODULE, Connect]),
    gen_server:cast(?MODULE, Connect).

disconnect(Disconnect) ->
    gen_server:cast(?MODULE, Disconnect).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% CALLBACKS %%

update_ref(Func, Tracker, Trackers) ->
    R = Func(Tracker#tracker.refs),
    Tracker2 = Tracker#tracker{refs = R},
    {R, Tracker, Trackers#{Tracker2#tracker.id := Tracker2}}.

inc_ref(Tracker, Trackers) ->
    F = fun(Refs) -> Refs + 1 end,
    update_ref(F, Tracker, Trackers).

dec_ref(Tracker, Trackers) ->
    F = fun(Refs) -> Refs - 1 end,
    update_ref(F, Tracker, Trackers).

add_tracker(Tracker = #tracker{id = ID}, Trackers) ->
    {ok, _Pid} = ?TRACKER_SUP:start_child(Tracker),
    Trackers2 = Trackers#{ID => Tracker},
    {Tracker, Trackers2}.

remove_tracker(Tracker = #tracker{id = ID}, Trackers) ->
    ok = ?TRACKER_SUP:terminate_child(Tracker),
    maps:remove(ID, Trackers).

gen_id(#tracker{host = Host, port = Port, proto = Proto}) ->
    A = list_to_binary(atom_to_list(Proto)),
    B = list_to_binary(Host),
    C = integer_to_binary(Port),
    ID = crypto:hash(sha, iolist_to_binary([A, B, C])),
    {ok, ID}.

init(_Args) ->
    lager:info("~s(~w): started", [?MODULE, self()]),
    {ok, #state{}, hibernate}.

handle_call(Request, From, State) ->
    lager:error("~s(~w): unhandled call '~w' '~s'",
                [?MODULE, self(), From, Request]),
    {stop, unhandled_call, State}.

handle_cast(#'tracker.srv.connect'{proto = Proto,
                                   host = Host,
                                   port = Port,
                                   reply = Reply},
            State = #state{trackers = Trackers}) ->
    lager:info("~s(~w): connect", [?MODULE, self()]),

    {ok, ID} = gen_id(#tracker{proto = Proto,
                               host = Host,
                               port = Port}),

    {Tracker, New_trackers} = try maps:get(ID, Trackers) of
        T ->
            {_Refs, Tracker2, Trackers2} = inc_ref(T, Trackers),
            {Tracker2, Trackers2}
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
    end,

    Reply ! #'tracker.srv.connected'{ tracker_worker_pid = Tracker#tracker.id },
    {noreply, State#state{trackers = New_trackers}, hibernate};
handle_cast(#'tracker.srv.disconnect'{ tracker_worker_pid = ID, reply = Reply },
            State = #state{trackers = Trackers}) ->
    #{ID := Tracker} = Trackers,
    {Refs, Tracker2, Trackers2} = dec_ref(Tracker, Trackers),

    Trackers3 = case Refs =:= 0 of
        false -> Trackers2;
        true -> remove_tracker(Tracker2, Trackers2)
    end,

    Reply ! #'tracker.srv.disconnected'{ tracker_worker_pid = ID },

    {noreply, State#state{trackers = Trackers3}, hibernate};
handle_cast(Request, State) ->
    lager:error("~s(~w): unhandled cast '~s'", [?MODULE, self(), Request]),
    {stop, unhandled_cast, State}.

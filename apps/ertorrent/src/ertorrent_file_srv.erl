-module(ertorrent_file_srv).

-behaviour(gen_server).

-export([pread/3,
         start_link/0,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {disks::list(),
                workers::list()}).

-define(FILE_SUP, ertorrent_file_sup).

pread(Info_hash, File, Offset) ->
    gen_server:cast(?MODULE, {pread, self(), Info_hash, File, Offset}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    %ok = application:start(sasl),
    %ok = application:start(os_mon),

    Disks = disksup:get_disk_data(),

    {ok, #state{disks = Disks}}.

terminate(_Reason, _State) ->
    %ok = application:stop(os_mon),
    %ok = application:stop(sasl),
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({pread, From, Info_hash, File, Offset}, State) ->
    case lists:keyfind(Info_hash, 2) of
        {ID, Info_hash} ->
            Worker = ID,
            New_workers = State#state.workers;
        false ->
            ID = erlang:unique_integer(),
            Worker = supervisor:start_child(?FILE_SUP, [ID, Info_hash]),
            New_workers = State#state{workers=[Worker | State#state.workers]}
    end,

    Worker ! {file_s_pread_req, From, File, Offset},

    New_state = State#state{workers = New_workers},

    {noreply, New_state}.

handle_info({file_w_pread_resp, _From, _File, _Offset, _Data}, State) ->
    {noreply, State}.

code_change(_OldVsn, _State, _Extra) ->
    {ok}.

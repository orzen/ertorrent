-module(ertorrent_file_worker).

-export([start_link/2,
         stop/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-include("ertorrent_log.hrl").

% This is NOT a thought through value
-define(DISPATCH_LIMIT, 10).

-record(state, {files::list(),
                info_hash,
                uid}).

% TODO figure out:
% - Keep fds open? Should each worker keep all some fds as state?
% - May be efficient to download pieces belonging to the same file?
% - Stack some pieces in memory before doing a pwrite/2 this will increase effciency?

start_link(Uid, Info_hash) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Uid, Info_hash], []).

stop() ->
    gen_server:stop(?MODULE).

init([Uid, Info_hash]) ->
    {ok, #state{info_hash=Info_hash, uid=Uid}}.

terminate(_Reason, _State) ->
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({pread, From, Filename, Begin, Length}, State) ->
    % TODO check if FD is open before reading or writing
    % TODO add timeout, if the process is inactive for 1-5 min, tear it down
    case file:open(Filename, [binary]) of
        {ok, Fd} ->
            case file:pread(Fd, Begin, Length) of
                {ok, Data} ->
                    From ! {file_w, {pread, Filename, Begin, Length}, Data};
                eof ->
                    From ! {file_w_error, {pread, Filename, Begin, Length}, eof};
                {error, Reason} ->
                    From ! {file_w_error, {pread, Filename, Begin, Length}, Reason}
            end;
        {error, Reason} ->
            ?WARNING("failed to open file: " ++ Filename ++ ", reason: " ++ Reason),
            %TODO make sure this goes to the file_srv
            From ! {file_w_error, {pread, Filename, Begin, Length}, Reason}
    end,

    {noreply, State};

handle_cast({pwrite, From, Filename, Offset, Data}, State) ->
    case file:open(Filename, [write]) of
        {ok, Fd} ->
            case file:pwrite(Fd, Offset, Data) of
                ok ->
                    From ! {file_w, {pwrite, Filename, Offset}};
                {error, Reason} ->
                    From ! {file_w_error, {pwrite, Filename, Offset, Data}, Reason}
            end,

            New_files = [{Filename, Fd}| State#state.files],

            New_state = State#state{files = New_files},

            {noreply, New_state};
        {error, Reason} ->
            {stop, Reason, State}
    end;
handle_cast(Req, State) ->
    ?WARNING("unhandled cast request: " ++ Req),
    {noreply, State}.

handle_info(Req, State) ->
    ?WARNING("unhandled info request: " ++ Req),
    {noreply, State}.

-module(ertorrent_file_worker).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% This is NOT a thought through value
-define(DISPATCH_LIMIT, 10).

-record(file, {name::string(),
               exists::boolean(),
               fd::io_device()})

-record(state, {files=[],
                info_hash,
                uid}).

% TODO figure out:
% - Keep fds open? Should each worker keep all some fds as state?
% - May be efficient to download pieces belonging to the same file?
% - Stack some pieces in memory before doing a pwrite/2 this will increase effciency?

open(Filename, Files) ->
    case lists:keyfind(Filename, 1, Files) of
        {Filename, _, Fd} ->
            {ok, Fd};
        false ->
            Exists = filelib:is_file(Filename),

            file:open(Filename, [write]),
    end,

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

handle_cast({pread, From, Offset, Filename}, State) ->
    % TODO check if FD is open before reading or writing
    % TODO add timeout, if the process is inactive for 1-5 min, tear it down
    case file:pread() of
        {ok, Data} -> ok;
        eof -> ok;
        {error, Reason} -> ok
    end,
    {noreply, State};

handle_cast({pwrite, From, Filename, Offset, Data}, State) ->
    Exists = filelib:is_file(Filename),
    case file:open(Filename, [write]) of
        {ok, Fd} ->
            {ok, #state{name=Filename, exists=Exists}};
        {error, Reason} ->
            {stop, Reason}
    end.
    case file:pwrite(Fd, Offset, Data) of
        ok ->
            From ! {piece_written, Filename},
        {error, Reason} ->
            From ! {piece_write_error, pwrite, Reason}
    end;

    {noreply, State};
handle_cast({tag, request}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, Extra) ->
    {ok, State}.

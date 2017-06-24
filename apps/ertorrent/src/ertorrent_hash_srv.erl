-module(ertorrent_hash_srv).

-include("ertorrent_log.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(debug_info).

-export([start/0,
         start_link/0,
         stop/0,
         hash_piece/2,
         hash_files/3,
         hash_piece_int/3,
         hash_piece_int/4,
         read/6,
         read_files/2]).

% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(job, {name::string(),
              self::pid(),
              from::pid(),
              filenames::list(),
              piece_length::integer(),
              n_pieces::integer(),
              pieces::list()}).

-record(state, {jobs::list()}).

% Used to hash individual pieces that has been received and will usually
% respond back to the calling process.
hash_piece_int(Pid, Id, Data) ->
    % Hash the binary part
    <<Hash_int:160/integer>> = crypto:hash(sha, Data),
    % Convert the hash to a string representation
    Hash = lists:flatten(io_lib:format("~40.16.0b", [Hash_int])),

    Pid ! {hashed_piece, {Id, Hash}}.

% Used to hash entire sets of files and will therefore usually respond back to
% itself to concatenate all the hashed pieces.
hash_piece_int(Pid, Name, Id, Data) ->
    % Hash the binary part
    <<Hash_int:160/integer>> = crypto:hash(sha, Data),
    % Convert the hash to a string representation
    Hash = lists:flatten(io_lib:format("~40.16.0b", [Hash_int])),

    Pid ! {hashed_piece, {Name, Id, Hash}}.

read(Job, Filename, _Fd, Piece_index, Read, Remaining) when Piece_index =/= 0
                                                       andalso byte_size(Read) < Job#job.piece_length ->
    case Filename =:= lists:last(Job#job.filenames) of
        true ->
            spawn(?MODULE, hash_piece_int, [Job#job.self, Job#job.name, Piece_index, Read]);
        false ->
            ok
    end,
    {Piece_index, Remaining};
read(Job, Filename, Fd, Piece_index, Read, Remaining) when is_binary(Remaining) ->
    Read_len = Job#job.piece_length - byte_size(Remaining),

    case byte_size(Read) > 0 of
        true ->
            spawn(?MODULE, hash_piece_int, [Job#job.self, Job#job.name, Piece_index, Read]);
        false ->
            []
    end,

    case file:read(Fd, Read_len) of
        {ok, Data} ->
            read(Job, Filename, Fd, Piece_index + 1, <<Remaining/binary, Data/binary>>, <<>>);
        eof ->
            ?WARNING("EOF occured");
        {error, Reason} ->
            ?WARNING("Error occured: " ++ Reason)
    end.

read_files(Job) ->
    lists:foldl(fun(Filename, {Piece_index, Remaining}) ->
                    case file:open(Filename, [binary]) of
                        {ok, Fd} ->

                            {Piece_index_new,
                             Remaining_new} = ?MODULE:read(Job,
                                                           Filename,
                                                           Fd,
                                                           Piece_index,
                                                           <<>>,
                                                           Remaining),

                            file:close(Fd),
                            {Piece_index_new, Remaining_new};
                        {error, enoent} ->
                            Job#job.from ! {hashed_files, {no_such_file, Filename}},
                            ?WARNING("no such file: " ++ Filename),
                            {Piece_index, Remaining}
                    end
                end, {0, <<>>}, Job#job.filenames).

start() ->
    gen_server:start(?MODULE, "", []).

stop() ->
    gen_server:stop(?MODULE).

% Id is used to tag the message so that the receiver will be able to match the
% request and response.
hash_piece(Id, Data) when is_binary(Data) ->
    gen_server:cast(?MODULE, {hash_piece, self(), Id, Data}).
hash_files(Id, Filenames, Piece_length) ->
    gen_server:cast(?MODULE, {hash_files, Id, self(), Filenames, Piece_length}).

% TODO maybe need start_link/4 for additional name if more than one server
% instance is needed
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?DEBUG("starting"),
    {ok, #state{jobs=[]}}.

% _Reason is defined as unused to avoid compilation warning when not building
% as debug.
terminate(_Reason, _State) ->
    ?DEBUG("shutting down: " ++ _Reason),
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({hash_piece, Pid, Id, Data}, State) ->
    hash_piece_int(Pid, Id, Data),

    {noreply, State};
handle_cast({hash_files, Name, From, Filenames, Piece_length}, State) when is_list(Filenames) ->
    File_sizes = lists:foldl(fun(Filename, Sum) ->
                                 filelib:file_size(Filename) + Sum
                             end, 0, Filenames),

    case File_sizes rem Piece_length of
        0 ->
            N_pieces = trunc(File_sizes/Piece_length);
        _ ->
            N_pieces = trunc(File_sizes/Piece_length) + 1
    end,
    ?DEBUG("file size: " ++ File_sizes ++ ", pieces: " ++ N_pieces),

    Job = #job{name=Name,
               self=self(),
               from=From,
               filenames=Filenames,
               piece_length=Piece_length,
               n_pieces=N_pieces,
               pieces=[]},

    New_state = State#state{jobs=[Job|State#state.jobs]},

    spawn(?MODULE, read_files, Job),

    {noreply, New_state}.

% Gathering all hashed pieces and send message back to the invoker.
% TODO Make sure to create all files initalized with zeros.
handle_info({hashed_piece, {Name, Piece_index, Hash}}, State) ->
    [Job] = [Job || #job{name=Name2} = Job <- State#state.jobs, Name =:= Name2],

    Old_pieces = Job#job.pieces,
    New_pieces = [{Piece_index, Hash}| Old_pieces],

    New_job = Job#job{pieces=New_pieces},

    case New_job#job.n_pieces =/= 0 andalso
         New_job#job.n_pieces == length(New_job#job.pieces) of
        true ->
            Sorted = lists:keysort(1, New_pieces),
            Pieces = [X || {_,X} <- Sorted],
            New_jobs = lists:keydelete(Name, 2, State#state.jobs),
            ?DEBUG("finished hashing " ++ Name),
            New_job#job.from ! {hashed_files, {Name, Pieces}};
        false ->
            New_jobs = lists:keyreplace(Name, 2, State#state.jobs, New_job)
    end,

    New_state = State#state{jobs=New_jobs},

    {noreply, New_state}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


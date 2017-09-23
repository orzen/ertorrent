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
         read_files/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(job, {id::integer(),
              name::string(), % ID for the hash-job
              self::pid(), % The PID of the server to get the results from the workers
              from::pid(), % The PID of the process that sent the hash request
              filenames::list(), % The files involved
              piece_length::integer(), % The length of a piece
              n_pieces::integer(), % The total number of pieces
              pieces::list()}). % The accumulated pieces

-record(state, {jobs::list()}).

start() ->
    gen_server:start(?MODULE, "", []).

% TODO maybe need start_link/4 for additional name if more than one server
% instance is needed
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

% Id is used to tag the message so that the receiver will be able to match the
% request and response.
hash_piece(Id, Data) when is_binary(Data) ->
    gen_server:cast(?MODULE, {hash_piece, self(), Id, Data}).

hash_files(Id, Filenames, Piece_length) ->
    lager:warning("hash files"),
    gen_server:cast(?MODULE, {hash_s_hash_files, Id, self(), Filenames, Piece_length}).

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
    lager:warning("JOB ~p", [Job]),
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
                            lager:warning("no such file: ~p", [Filename]),
                            Job#job.from ! {hashed_files, {no_such_file, Filename}},
                            {Piece_index, Remaining}
                    end
                end, {0, <<>>}, Job#job.filenames).

init(_Args) ->
    lager:warning("starting hash_srv", []),
    {ok, #state{jobs=[]}, hibernate}.

fork_hashing(Job_ID, Index, Data) ->
    Hash_fun = fun() ->
                   % Hash the binary part
                   <<Hash_int:160/integer>> = crypto:hash(sha, Data),
                   % Convert the hash to a string representation
                   Hash = lists:flatten(io_lib:format("~40.16.0b", [Hash_int])),

                   self() ! {hashed_piece2, {Job_ID, Index, Hash}}
               end,

    spawn(Hash_fun).

% From, a pid to send the response to once a hashing job is completed
% Job_ID, a name to collect all the hashed pieces into
% Mapping, a list with tuples that can translate between piece index and file information

% Iterating through all the pieces that belongs to a file that does not exist
% and send an empty string as hash response.
skip_hashing_file(Job_ID, {Index, File_path, File_offset, Length}, Rest) ->
    Filter = fun({_Index_x, File_path_x, _File_offset_x, _Length_x}) ->
                     File_path =:= File_path_x
             end,

    Missing_pieces = lists:filter(Filter, Rest),
    Missing_incl_first = [{Index, File_path, File_offset, Length}| Missing_pieces],

    Foreach = fun({Index_y, _File_path, _File_offset, _Length}) ->
                  % Send back an empty hash string since the piece is missing.
                  self() ! {hashed_piece, Job_ID, Index_y, ""}
              end,

    % Send the message for all the missing pieces incl. the first that got
    % poped in the previous function.
    lists:foreach(Foreach, Missing_incl_first),

    % The piece that got sent seperately is already removed from Rest so just
    % remove the remaining pieces that belongs to the missing file.
    New_mapping = Rest -- Missing_pieces,

    {ok, New_mapping}.


ensure_fd(File_path, FDs) ->
    % Check if the FD is already open
    case lists:keyfind(File_path, 1, FDs) of
        {File_path, Fd} ->
            {ok, Fd, FDs};
        false ->
            case file:open(File_path, [binary]) of
                {ok, Fd} ->
                    {ok, Fd, [{File_path, Fd}| FDs]};
                {error, enoent} ->
                    {error, enoent};
                {error, Reason} ->
                    lager:error("failed to open file: '~p'", [Reason])
            end
    end.

% No work left to be done, except closing the IO devices.
hash_files2(_Job_ID, [], IO_devs) ->
    % TODO close the IO_devs
    Foreach = fun({_File_path, IO_dev}) ->
                  file:close(IO_dev)
              end,

    lists:foreach(Foreach, IO_devs);
% Read a single piece and spawn a hashing process. This clause should be the
% one called the most.
hash_files2(Job_ID, [{Index, File_path, File_offset, Length}| Rest], IO_devs) ->
    case ensure_fd(File_path, IO_devs) of
        {ok, FD, New_IO_devs} ->
            % Read a piece from the file
            case file:read(FD, Length) of
                {ok, Data} ->
                    % If the read is successful spawn a process that will do
                    % the hashing and message back with the result.
                    fork_hashing(Job_ID, Index, Data),

                    % Move on to the next piece
                    hash_files(Job_ID, Rest, New_IO_devs);
                eof ->
                    % Don't know if this will occur, leaving a log entry if it does.
                    lager:warning("reached eof");
                {error, Reason} ->
                    % This should not occur, so if it does, log it!
                    lager:error("failed to read offset: '~p'", [Reason])
            end;
        {error, enoent} ->
            % There is no need to do work if the file does not exist. Iterate
            % through all the pieces that is related to the missing file
            % (excluding overlapping pieces) and respond with an empty string.
            New_mapping = skip_hashing_file(Job_ID, {Index, File_path, File_offset, Length}, Rest),
            hash_files(Job_ID, New_mapping, IO_devs)
    end;
% This clause will handle the cases when a file is overlapping with another
% file to create one piece.
hash_files2(Job_ID, [{Index,
                    [{File_path_x, File_offset_x, Length_x},
                     {File_path_y, File_offset_y, Length_y}]}| Rest], IO_devs) ->
    case ensure_fd(File_path_x, IO_devs) of
        {ok, Fd_x, New_IO_devs_x} ->
            New_IO_devs = New_IO_devs_x,

            % Read a piece from the file
            case file:read(Fd_x, Length_x) of
                {ok, Data_x} ->
                    Data_x;
                eof ->
                    % Don't know if this will occur, leaving a log entry if it does.
                    lager:warning("reached eof"),
                    Data_x = <<>>;
                {error, Reason_x} ->
                    % This should not occur, so if it does, log it!
                    lager:error("failed to read offset: '~p'", [Reason_x]),
                    Data_x = <<>>
            end,

            New_rest = Rest;
        {error, enoent} ->
            % There is no need to do work if the file does not exist. Iterate
            % through all the pieces that is related to the missing file
            % (excluding overlapping pieces) and respond with an empty string.
            % TODO Verify if this is necessary. Since _this_ part of the
            % overlapping piece is located at the end of a file it should not
            % have any effect to skip the rest of the pieces in the same file.
            New_mapping_x = skip_hashing_file(Job_ID,
                                              {Index,
                                               File_path_x,
                                               File_offset_x,
                                               Length_x},
                                              Rest),
            New_rest = New_mapping_x,
            New_IO_devs = IO_devs,
            Data_x = <<>>
    end,
    case ensure_fd(File_path_y, New_IO_devs) of
        {ok, Fd_y, New_IO_devs_y} ->
            % Read a piece from the file
            case file:read(Fd_y, Length_y) of
                {ok, Data_y} ->
                    % If the read is successful spawn a process that will do
                    % the hashing and message back with the result.
                    fork_hashing(Job_ID, Index, <<Data_x/binary, Data_y/binary>>),

                    % Move on to the next piece
                    hash_files(Job_ID, New_rest, New_IO_devs_y);
                eof ->
                    % Don't know if this will occur, leaving a log entry if it does.
                    lager:warning("reached eof");
                {error, Reason_y} ->
                    % This should not occur, so if it does, log it!
                    lager:error("failed to read offset: '~p'", [Reason_y])
            end;
        {error, enoent} ->
            % There is no need to do work if the file does not exist. Iterate
            % through all the pieces that is related to the missing file
            % (excluding overlapping pieces) and respond with an empty string.
            New_mapping_y = skip_hashing_file(Job_ID,
                                              {Index,
                                               File_path_y,
                                               File_offset_y,
                                               Length_y},
                                              New_rest),

            % Move on to the next file
            hash_files(Job_ID, New_mapping_y, New_IO_devs)
    end.


% _Reason is defined as unused to avoid compilation warning when not building
% as debug.
terminate(_Reason, _State) ->
    ?DEBUG("shutting down: " ++ _Reason),
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

%% @doc Callback for the client API to hash a single piece by its data
handle_cast({hash_piece, Pid, Id, Data}, State) ->
    hash_piece_int(Pid, Id, Data),

    {noreply, State};

handle_cast({hash_s_hash_files2, From, Job_ID, Mapping}, State) ->
    % Create a new job
    Job = #job{id = Job_ID,
               from = From,
               n_pieces = length(Mapping),
               pieces = []},

    % Add the new job to the list of jobs
    New_jobs = [Job| State#state.jobs],

    % Updating the state with the new job list
    New_state = State#state{jobs = New_jobs},

    % Start the actual work, the outcome of this work will come back as
    % messages.
    hash_files2(Job_ID, Mapping, []),

    {noreply, New_state};

%% @doc Callback for the client API to calculate the hashes for a list of files
handle_cast({hash_s_hash_files, Name, From, Filenames, Piece_length}, State) when is_list(Filenames) ->
    lager:warning("BEGINNING"),
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
               filenames=Filenames, % not necessary
               piece_length=Piece_length, % not necessary
               n_pieces=N_pieces, % not necessary
               pieces=[]},

    New_state = State#state{jobs=[Job|State#state.jobs]},
    lager:warning("BEFORE SPAWN"),

    spawn(?MODULE, read_files, [Job]),

    lager:warning("END"),

    {noreply, New_state}.

handle_info({hashed_piece2, {Job_ID, Index, Hash}}, State) ->
    lager:debug("hashed_piece2"),

    case lists:keyfind(Job_ID, #job.id, State#state.jobs) of
        false ->
            lager:error("failed to find the job for a hashed piece"),
            New_state = State;
        Job ->
            % Add the newly hashed piece to the list of pieces
            New_job = Job#job{pieces = [{Index, Hash}| Job#job.pieces]},

            % Check if the job got the expected amount of pieces
            case Job#job.n_pieces =:= length(New_job#job.pieces) of
                false ->
                    % Replace the current job with the update job
                    New_jobs = lists:keyreplace(Job_ID, #job.id, State#state.jobs, New_job);
                true ->
                    % Sort by piece index
                    Sorted = lists:keysort(1, New_job#job.pieces),

                    % Create a list with the ordered hashes without the indecies
                    Pieces = [X || {_,X} <- Sorted],

                    % Remove the completed job from the list
                    New_jobs = lists:keydelete(Job_ID, #job.id, State#state.jobs),

                    lager:debug("finished hashing '~p'", Job_ID),

                    % Send the hash response
                    New_job#job.from ! {hash_s_hashed_files, {Job_ID, Pieces}}
            end,

            % Update the state with the new job list
            New_state = State#state{jobs = New_jobs}
    end,

    {noreply, New_state};

% Gathering all hashed pieces and send message back to the invoker.
% TODO Make sure to create all files initalized with zeros.
handle_info({hashed_piece, {Name, Piece_index, Hash}}, State) ->
    [Job] = [Job || #job{name=Name2} = Job <- State#state.jobs, Name =:= Name2],

    Old_pieces = Job#job.pieces,
    New_pieces = [{Piece_index, Hash}| Old_pieces],

    New_job = Job#job{pieces=New_pieces},

    % TODO probably could remove the guard '=/= 0'
    case New_job#job.n_pieces =/= 0 andalso
         New_job#job.n_pieces == length(New_job#job.pieces) of
        true ->
            % Sort by piece index
            Sorted = lists:keysort(1, New_pieces),

            % Create a list with the ordered hashes
            Pieces = [X || {_,X} <- Sorted],

            % Remove the completed job from the list
            New_jobs = lists:keydelete(Name, 2, State#state.jobs),
            ?DEBUG("finished hashing " ++ Name),
            New_job#job.from ! {hash_s_hashed_files, {Name, Pieces}};
        false ->
            New_jobs = lists:keyreplace(Name, 2, State#state.jobs, New_job)
    end,

    New_state = State#state{jobs=New_jobs},

    {noreply, New_state}.

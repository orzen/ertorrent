-module(ertorrent_hash_srv).

-include("ertorrent_log.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(debug_info).

-export([start/0,
         start_link/0,
         stop/0,
         hash_piece/2,
         hash_files/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(job, {id::integer(),
              from::pid(), % The PID of the process that sent the hash request
              n_pieces::integer(), % The total number of pieces
              pieces::list()}). % The accumulated pieces

-record(state, {jobs::list()}).

%%% CLIENT API %%%

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

% hash_files(Id, Filenames, Piece_length) ->
%     lager:warning("hash files"),
%     gen_server:cast(?MODULE, {hash_s_hash_files, Id, self(), Filenames, Piece_length}).

hash_files(File_mapping) ->
    Job_ID = erlang:unique_integer(),
    gen_server:cast(?MODULE, {hash_s_hash_files, self(), Job_ID, File_mapping}),
    {ok, Job_ID}.

%%% INTERNAL FUNCTIONS %%%

fork_hashing(Job_ID, Index, Data) ->
    Hash_fun = fun() ->
                   % Hash the binary part
                   <<Hash_int:160/integer>> = crypto:hash(sha, Data),
                   % Convert the hash to a string representation
                   Hash = lists:flatten(io_lib:format("~40.16.0b", [Hash_int])),

                   self() ! {collect_hashed_pieces, {Job_ID, Index, Hash}}
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
                  self() ! {collect_hashed_pieces, {Job_ID, Index_y, ""}}
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
hash_files_int(_Job_ID, [], IO_devs) ->
    % TODO close the IO_devs
    Foreach = fun({_File_path, IO_dev}) ->
                  file:close(IO_dev)
              end,

    lists:foreach(Foreach, IO_devs);
% Read a single piece and spawn a hashing process. This clause should be the
% one called the most.
hash_files_int(Job_ID, [{Index, File_path, File_offset, Length}| Rest], IO_devs) ->
    case ensure_fd(File_path, IO_devs) of
        {ok, FD, New_IO_devs} ->
            % Read a piece from the file
            case file:read(FD, Length) of
                {ok, Data} ->
                    % If the read is successful spawn a process that will do
                    % the hashing and message back with the result.
                    fork_hashing(Job_ID, Index, Data),

                    % Move on to the next piece
                    hash_files_int(Job_ID, Rest, New_IO_devs);
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
            {ok, New_mapping} = skip_hashing_file(Job_ID, {Index, File_path, File_offset, Length}, Rest),
            hash_files_int(Job_ID, New_mapping, IO_devs)
    end;
% This clause will handle the cases when a file is overlapping with another
% file to create one piece.
hash_files_int(Job_ID, [{Index,
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
            {ok, New_mapping_x} = skip_hashing_file(Job_ID,
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
                    hash_files_int(Job_ID, New_rest, New_IO_devs_y);
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
            {ok, New_mapping_y} = skip_hashing_file(Job_ID,
                                                    {Index,
                                                     File_path_y,
                                                     File_offset_y,
                                                     Length_y},
                                                    New_rest),

            % Move on to the next file
            hash_files_int(Job_ID, New_mapping_y, New_IO_devs)
    end.

init(_Args) ->
    lager:debug("~p: '~p'", [?MODULE, ?FUNCTION_NAME]),
    {ok, #state{jobs=[]}, hibernate}.

% _Reason is defined as unused to avoid compilation warning when not building
% as debug.
terminate(Reason, _State) ->
    lager:debug("shutting down: '~p'", [Reason]),
    ok.

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({hash_s_hash_files, From, Job_ID, Mapping}, State) ->
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
    hash_files_int(Job_ID, Mapping, []),

    {noreply, New_state}.

handle_info({collect_hashed_pieces, {Job_ID, Index, Hash}}, State) ->
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
                    Piece_hashes = [X || {_,X} <- Sorted],

                    % Remove the completed job from the list
                    New_jobs = lists:keydelete(Job_ID, #job.id, State#state.jobs),

                    lager:debug("finished hashing '~p'", [Job_ID]),

                    % Send the hash response
                    New_job#job.from ! {hash_s_hash_files_res, {Job_ID, Piece_hashes}}
            end,

            % Update the state with the new job list
            New_state = State#state{jobs = New_jobs}
    end,

    {noreply, New_state}.

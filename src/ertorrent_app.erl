-module(ertorrent_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->

    %%%  COWBOY & REBAR3 TUTORIAL %%%
    % URL: https://gist.github.com/flbuddymooreiv/ce1d7a47b12c27bf1616
    %{ok, Pid} = 'cowboy_hello_world_sup':start_link(),

    Routes = [ {
        '_',
        [
            {"/", ertorrent_rest_v1_top, []}
        ]
    } ],
    Dispatch = cowboy_router:compile(Routes),

    TransOpts = [{port, 8080}],

    ProtoOpts = #{
        env => #{dispatch => Dispatch}
    },

    {ok, _} = cowboy:start_clear(http,
                                 TransOpts,
                                 ProtoOpts),

    %%% END OF COWBOY TUTORIAL SNIPPET %%%

    % Configuration handling, distributed setup and OS-specifics
    % ertorrent_sup:start_link_one(),


    % The rest of the internals
    %ertorrent_sup:start_link_two(),

    % The API
    %ertorrent_sup:start_link_three().

    % Start up the servers
    {ok, Pid} = ertorrent_sup:start_link(),

    % This also comes from the Cowboy tutorial
    {ok, Pid}.

-spec stop(_) -> ok.
stop(_State) ->
    ok.

-ifdef(TEST).

dummy_test() ->
    1.

-endif.

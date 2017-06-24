-module(ertorrent_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % Configuration handling, distributed setup and OS-specifics
    ertorrent_sup:start_link_one().

    % The rest of the internals
    ertorrent_sup:start_link_two().

    % The API
    ertorrent_sup:start_link_three().

stop(_State) ->
    ok.

-ifdef(TEST).

dummy_test() ->
	1.

-endif.

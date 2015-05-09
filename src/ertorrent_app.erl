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
    ertorrent_sup:start_link().

stop(_State) ->
    ok.

-ifdef(TEST).

dummy_test() ->
	1.

-endif.

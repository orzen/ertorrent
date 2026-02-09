%%%-------------------------------------------------------------------
%% @doc ertorrent public API
%% @end
%%%-------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-module(ertorrent_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	% Set default logger without changing the handler
	logger:set_primary_config(level, debug),

	% Log to console (this did not work as intended)
	%logger:set_handler_config(default, level, debug),
	?LOG_DEBUG("Make sure that debug logs work"),

	ertorrent_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

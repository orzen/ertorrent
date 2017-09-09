-module(ertorrent_rest_v1_top).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2,
         test_json/2,
         test_plain/2
         ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application", "json">>, []}, test_json},
        {{<<"text", "plain">>, []}, test_plain}
     ], Req, State}.

test_json(Req, State) ->
    Body = <<"{\"msg\" : \"Hello ertorrent!!\"}">>,

    {Body, Req, State}.

test_plain(Req, State) ->
    Body = <<"Hello ertorrent!!">>,

    {Body, Req, State}.

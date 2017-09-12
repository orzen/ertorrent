-module(ertorrent_rest_v1_top).

-export([
         top_html/2,
         top_json/2,
         top_plain/2
        ]).

-export([
         init/2,
         allowed_methods/2,
         content_types_provided/2
        ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, top_html},
        {<<"application/json">>, top_json},
        {<<"text/plain">>, top_plain}
     ], Req, State}.

top_html(Req, State) ->
    Body = <<"<html>
<head>
    <meta charset=\"utf-8\">
    <title>REST Hello World!</title>
</head>
<body>
    <p>REST Hello World as HTML!</p>
</body>
</html>">>,

    {ok, Metainfo} = ertorrent_metainfo:parse_file("/home/orz/downloads/debian-9.1.0-amd64-netinst.iso.torrent"),
    ertorrent_torrent_srv:add(Metainfo),

    {Body, Req, State}.

top_json(Req, State) ->
    Body = <<"{\"msg\" : \"Hello ertorrent!!\"}">>,

    {Body, Req, State}.

top_plain(Req, State) ->
    Body = <<"Hello ertorrent!!">>,

    {Body, Req, State}.

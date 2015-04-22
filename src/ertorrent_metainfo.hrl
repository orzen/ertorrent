-record('metainfo.magnet', {
    name :: bitstring(),
    info_hash :: bitstring(),
    announce_list = []
}).

-record('metainfo.file', {
    info, % Record 'torrent.meta.file.info'
    info_hash :: bitstring(),
    announce :: string(),
    announce_list = [],
    creation_date :: integer(),
    comment :: string(),
    created_by :: string(),
    encoding :: string()
}).

-record('metainfo.file.info', {
    piece_length :: integer(),
    pieces,
    private,
    mode % SEE MODES BELOW
}).

% Single file mode
-record('metainfo.file.info.sf', {
    name :: string(),
    length :: integer(),
    md5sum
}).

% Multiple file mode
-record('metainfo.file.info.mf', {
    name :: string(),
    files :: list()
}).

%%% mpd_state - gen_server owning all mutable state + music database.
%%%
%%% This is the heart of the actor design: instead of a mutex around global
%%% predicates, all state lives in ONE process.  Reads and writes are both
%%% messages to this process, serialised by the Erlang VM's message queue.

-module(mpd_state).
-behaviour(gen_server).

%% API
-export([start_link/0,
         status/0, stats/0,
         play/0, play/1, stop/0, pause/0, next/0, previous/0,
         add/1, delete/1, clear/0, playlistinfo/0, currentsong/0,
         setvol/1, set_single/1, set_consume/1,
         subscribe/1, unsubscribe/1,
         lsinfo/1, list/3, find/2, search/2, count/1, update/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% --- records ---
-record(song, {file    :: binary(),
               url     :: binary(),
               id      :: pos_integer(),
               tags    :: [{binary(), binary()}],
               pos     :: integer() | undefined}).

-record(db_entry, {
          file    :: binary(),          %% path relative to music root
          tags    :: [{binary(), binary()}]
         }).

-record(player, {song_id  :: pos_integer() | undefined,
                 state    :: play | pause | stop,
                 elapsed  :: float(),
                 duration :: float()}).

-record(mpd_state, {
          queue          = []     :: [#song{}],
          queue_version  = 0      :: integer(),
          next_id        = 1      :: pos_integer(),
          player         = undefined :: undefined | #player{},
          volume         = 50     :: integer(),
          single         = 0      :: 0 | 1,
          consume        = 0      :: 0 | 1,
          updating_db    = undefined :: undefined | pos_integer(),
          error          = undefined :: undefined | binary(),
          subscribers    = []     :: [pid()],
          start_time     = 0      :: integer(),
          db             = []     :: [#db_entry{}]
         }).

%% --- API functions ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% reading state
status()        -> gen_server:call(?MODULE, status).
stats()         -> gen_server:call(?MODULE, stats).
playlistinfo()  -> gen_server:call(?MODULE, playlistinfo).
currentsong()   -> gen_server:call(?MODULE, currentsong).

%% playback control
play()          -> gen_server:call(?MODULE, play).
play(Pos)       -> gen_server:call(?MODULE, {play_pos, Pos}).
stop()          -> gen_server:call(?MODULE, stop).
pause()         -> gen_server:call(?MODULE, pause).
next()          -> gen_server:call(?MODULE, next).
previous()      -> gen_server:call(?MODULE, previous).

%% queue mutation
add(Path)       -> gen_server:call(?MODULE, {add, Path}).
delete(Pos)     -> gen_server:call(?MODULE, {delete, Pos}).
clear()         -> gen_server:call(?MODULE, clear).

%% options
setvol(V)       -> gen_server:call(?MODULE, {setvol, V}).
set_single(V)   -> gen_server:call(?MODULE, {single, V}).
set_consume(V)  -> gen_server:call(?MODULE, {consume, V}).

%% subscriptions
subscribe(Pid)      -> gen_server:cast(?MODULE, {subscribe, Pid}).
unsubscribe(Pid)    -> gen_server:cast(?MODULE, {unsubscribe, Pid}).

%% database
lsinfo(Path)   -> gen_server:call(?MODULE, {lsinfo, Path}).
list(Type, Filter, GroupBy) -> gen_server:call(?MODULE, {list, Type, Filter, GroupBy}).
find(Exact, Filter) -> gen_server:call(?MODULE, {find, Exact, Filter}).
search(Exact, Filter) -> gen_server:call(?MODULE, {search, Exact, Filter}).
count(Filter)  -> gen_server:call(?MODULE, {count, Filter}).
update(Path)   -> gen_server:call(?MODULE, {update, Path}).

%% --- gen_server callbacks ---

init([]) ->
    {ok, #mpd_state{start_time = erlang:system_time(second), db = fake_db()}}.

%% --- status / stats / playlistinfo ---

handle_call(status, _From, S) ->
    {reply, {ok, status_reply(S)}, S};

handle_call(stats, _From, S) ->
    Now = erlang:system_time(second),
    Stats = [
        {<<"uptime">>, Now - S#mpd_state.start_time},
        {<<"songs">>, length(S#mpd_state.db)},
        {<<"db_playtime">>, db_total_duration(S)},
        {<<"db_update">>, S#mpd_state.start_time},
        {<<"playlist">>, S#mpd_state.queue_version},
        {<<"playlistlength">>, length(S#mpd_state.queue)},
        {<<"artists">>, db_distinct(<<"Artist">>, S)},
        {<<"albums">>, db_distinct(<<"Album">>, S)}
    ],
    {reply, {ok, Stats}, S};

handle_call(playlistinfo, _From, S) ->
    Songs = [song_info(I, Song) || {I, Song} <- lists:zip(lists:seq(0, length(S#mpd_state.queue)-1), S#mpd_state.queue)],
    {reply, {ok, Songs}, S};

handle_call(currentsong, _From, #mpd_state{queue = Q, player = #player{song_id = Id}} = S) ->
    case find_by_id(Id, Q) of
        {ok, Pos, Song} ->
            {reply, {ok, song_info(Pos, Song)}, S};
        none ->
            {reply, {ok, []}, S}
    end;

handle_call(currentsong, _From, S) ->
    {reply, {ok, []}, S};

%% --- playback ---

handle_call(play, _From, #mpd_state{queue = [], player = _P} = S) ->
    {reply, {error, <<"queue is empty">>}, S};

handle_call(play, _From, #mpd_state{queue = [Song | _], player = _P} = S) ->
    NewPlayer = #player{song_id = Song#song.id, state = play, elapsed = 0.0, duration = 0.0},
    notify_player(NewPlayer),
    notify_subscribers(<<"player">>, S),
    {reply, ok, S#mpd_state{player = NewPlayer}};

handle_call({play_pos, Pos}, _From, S) when Pos >= 0, Pos < length(S#mpd_state.queue) ->
    Song = lists:nth(Pos+1, S#mpd_state.queue),
    NewPlayer = #player{song_id = Song#song.id, state = play, elapsed = 0.0, duration = 0.0},
    notify_player(NewPlayer),
    notify_subscribers(<<"player">>, S),
    {reply, ok, S#mpd_state{player = NewPlayer}};

handle_call({play_pos, _Pos}, _From, S) ->
    {reply, {error, <<"bad position">>}, S};

handle_call(stop, _From, S) ->
    notify_player(stop),
    NewS = S#mpd_state{player = undefined},
    notify_subscribers(<<"player">>, NewS),
    {reply, ok, NewS};

handle_call(pause, _From, #mpd_state{player = #player{state = play} = P} = S) ->
    NewP = P#player{state = pause},
    notify_player(NewP),
    notify_subscribers(<<"player">>, S),
    {reply, ok, S#mpd_state{player = NewP}};

handle_call(pause, _From, #mpd_state{player = #player{state = pause} = P} = S) ->
    NewP = P#player{state = play},
    notify_player(NewP),
    notify_subscribers(<<"player">>, S),
    {reply, ok, S#mpd_state{player = NewP}};

handle_call(pause, _From, S) ->
    {reply, {error, <<"nothing playing">>}, S};

handle_call(next, _From, #mpd_state{queue = Q, player = P} = S) ->
    case find_next(P, Q) of
        {ok, Pos, Song} ->
            NewPlayer = #player{song_id = Song#song.id, state = play, elapsed = 0.0, duration = 0.0},
            notify_player(NewPlayer),
            NewS = maybe_consume_prev(Pos, S#mpd_state{player = NewPlayer}),
            notify_subscribers(<<"player">>, NewS),
            {reply, ok, NewS};
        none ->
            notify_player(stop),
            NewS = S#mpd_state{player = undefined},
            notify_subscribers(<<"player">>, NewS),
            {reply, ok, NewS}
    end;

handle_call(previous, _From, #mpd_state{queue = Q, player = P} = S) ->
    case find_prev(P, Q) of
        {ok, _Pos, Song} ->
            NewPlayer = #player{song_id = Song#song.id, state = play, elapsed = 0.0, duration = 0.0},
            notify_player(NewPlayer),
            NewS = S#mpd_state{player = NewPlayer},
            notify_subscribers(<<"player">>, NewS),
            {reply, ok, NewS};
        none ->
            {reply, {error, <<"no previous song">>}, S}
    end;

%% --- queue mutation ---

handle_call({add, Path}, _From, S) ->
    case lookup_db(Path, S) of
        {ok, Entry} ->
            Id = S#mpd_state.next_id,
            Song = #song{file = Entry#db_entry.file,
                         url = <<"stub://", Path/binary>>,
                         id = Id,
                         tags = Entry#db_entry.tags,
                         pos = undefined},
            NewQ = S#mpd_state.queue ++ [Song],
            Ver = S#mpd_state.queue_version + 1,
            notify_subscribers(<<"playlist">>, S),
            {reply, {ok, Id}, S#mpd_state{queue = NewQ, queue_version = Ver, next_id = Id+1}};
        not_found ->
            %% fallback: treat Path as a raw file
            Id = S#mpd_state.next_id,
            Song = #song{file = Path,
                         url = <<"stub://", Path/binary>>,
                         id = Id,
                         tags = [{<<"Title">>, filename:basename(Path)},
                                 {<<"Artist">>, <<"Unknown">>}],
                         pos = undefined},
            NewQ = S#mpd_state.queue ++ [Song],
            Ver = S#mpd_state.queue_version + 1,
            notify_subscribers(<<"playlist">>, S),
            {reply, {ok, Id}, S#mpd_state{queue = NewQ, queue_version = Ver, next_id = Id+1}}
    end;

handle_call({delete, Pos}, _From, S) when Pos >= 0, Pos < length(S#mpd_state.queue) ->
    NewQ = delete_at(Pos, S#mpd_state.queue),
    NewPlayer = adjust_player_delete(Pos, S#mpd_state.player, NewQ),
    Ver = S#mpd_state.queue_version + 1,
    notify_subscribers(<<"playlist">>, S),
    {reply, ok, S#mpd_state{queue = NewQ, queue_version = Ver, player = NewPlayer}};

handle_call({delete, _}, _From, S) ->
    {reply, {error, <<"bad position">>}, S};

handle_call(clear, _From, S) ->
    notify_player(stop),
    NewS = S#mpd_state{queue = [], queue_version = S#mpd_state.queue_version + 1, player = undefined},
    notify_subscribers(<<"playlist">>, NewS),
    {reply, ok, NewS};

%% --- options ---

handle_call({setvol, V}, _From, S) when V >= 0, V =< 100 ->
    mpd_player:set_volume(V),
    notify_subscribers(<<"mixer">>, S),
    {reply, ok, S#mpd_state{volume = V}};

handle_call({setvol, _}, _From, S) ->
    {reply, {error, <<"volume out of range">>}, S};

handle_call({single, V}, _From, S) when V =:= 0; V =:= 1 ->
    {reply, ok, S#mpd_state{single = V}};

handle_call({consume, V}, _From, S) when V =:= 0; V =:= 1 ->
    {reply, ok, S#mpd_state{consume = V}};

%% --- database ---

handle_call({lsinfo, <<>>}, _From, S) ->
    %% root directory: list top-level artists
    Dirs = db_top_dirs(S),
    Pairs = [{<<"directory">>, D} || D <- Dirs],
    {reply, {ok, Pairs}, S};

handle_call({lsinfo, Path}, _From, S) ->
    case db_list_dir(Path, S) of
        {dirs, Dirs} ->
            Pairs = [{<<"directory">>, D} || D <- Dirs],
            {reply, {ok, Pairs}, S};
        {files, Files} ->
            AllPairs = [db_entry_pairs(E) || E <- Files],
            {reply, {ok, lists:append(AllPairs)}, S};
        not_found ->
            {reply, {error, <<"not found">>}, S}
    end;

handle_call({list, Type, Filter, GroupBy}, _From, S) ->
    {reply, {ok, db_list(Type, Filter, GroupBy, S)}, S};

handle_call({find, Exact, Filter}, _From, S) ->
    {reply, {ok, db_find(Exact, Filter, S)}, S};

handle_call({search, Exact, Filter}, _From, S) ->
    {reply, {ok, db_search(Exact, Filter, S)}, S};

handle_call({count, Filter}, _From, S) ->
    Matches = db_match(Filter, S),
    TotalDur = lists:sum([tag_int(<<"duration">>, E#db_entry.tags, 0) || E <- Matches]),
    {reply, {ok, [{<<"songs">>, length(Matches)}, {<<"playtime">>, TotalDur}]}, S};

handle_call({update, _Path}, _From, S) ->
    UpId = S#mpd_state.next_id,
    %% fake: just bump a counter - in real system this would refresh the DB
    {reply, {ok, [{<<"updating_db">>, UpId}]}, S#mpd_state{updating_db = UpId}}.

%% --- casts ---

handle_cast({subscribe, Pid}, S) ->
    NewSubs = [Pid | lists:delete(Pid, S#mpd_state.subscribers)],
    {noreply, S#mpd_state{subscribers = NewSubs}};

handle_cast({unsubscribe, Pid}, S) ->
    {noreply, S#mpd_state{subscribers = lists:delete(Pid, S#mpd_state.subscribers)}}.

%% --- info ---

handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
    {noreply, S#mpd_state{subscribers = lists:delete(Pid, S#mpd_state.subscribers)}};

handle_info(_Msg, S) ->
    {noreply, S}.

%% ============================================================================
%% internal helpers — playback
%% ============================================================================

status_reply(S) ->
    #mpd_state{queue = Q, queue_version = V, player = P,
               volume = Vol, single = Sg, consume = Cons, error = Err} = S,
    Base = [{<<"volume">>, Vol},
            {<<"repeat">>, 0},
            {<<"random">>, 0},
            {<<"single">>, Sg},
            {<<"consume">>, Cons},
            {<<"playlist">>, V},
            {<<"playlistlength">>, length(Q)},
            {<<"state">>, player_state_atom(P)}],
    case Err of
        undefined -> Base;
        _ -> [{<<"error">>, Err} | Base]
    end.

player_state_atom(undefined) -> <<"stop">>;
player_state_atom(#player{state = play}) -> <<"play">>;
player_state_atom(#player{state = pause}) -> <<"pause">>;
player_state_atom(#player{state = stop}) -> <<"stop">>.

song_info(Pos, #song{file = F, tags = Tags, id = Id}) ->
    [{<<"file">>, F},
     {<<"Pos">>, integer_to_binary(Pos)},
     {<<"Id">>, integer_to_binary(Id)}
     | Tags].

db_entry_pairs(#db_entry{file = F, tags = Tags}) ->
    [{<<"file">>, F} | Tags].

notify_player(stop) -> mpd_player:stop();
notify_player(#player{state = play, song_id = Id}) -> mpd_player:play(Id);
notify_player(#player{state = pause}) -> mpd_player:pause().

notify_subscribers(Subsystem, #mpd_state{subscribers = Subs}) ->
    [Pid ! {mpd_changed, Subsystem} || Pid <- Subs],
    ok.

find_next(undefined, [S|_]) -> {ok, 0, S};
find_next(#player{song_id = Id}, Q) ->
    case lists:dropwhile(fun(#song{id = I}) -> I =/= Id end, Q) of
        [_ | [Next | _]] ->
            Pos = position_of(Next#song.id, Q),
            {ok, Pos, Next};
        _ -> none
    end.

find_prev(#player{song_id = Id}, Q) ->
    PrevList = lists:takewhile(fun(#song{id = I}) -> I =/= Id end, Q),
    case lists:reverse(PrevList) of
        [Prev | _] -> {ok, position_of(Prev#song.id, Q), Prev};
        [] -> none
    end.

find_by_id(Id, Q) ->
    case position_of(Id, Q) of
        Pos when is_integer(Pos) -> {ok, Pos, lists:nth(Pos + 1, Q)};
        undefined -> none
    end.

position_of(Id, Q) -> position_of(Id, Q, 0).
position_of(Id, [#song{id = Id} | _], Pos) -> Pos;
position_of(Id, [_ | Rest], Pos) -> position_of(Id, Rest, Pos + 1);
position_of(_, [], _) -> undefined.

delete_at(Pos, Q) ->
    {Front, [_ | Back]} = lists:split(Pos, Q),
    Front ++ Back.

adjust_player_delete(_Pos, undefined, _) -> undefined;
adjust_player_delete(_Pos, #player{song_id = Id} = P, Q) ->
    case position_of(Id, Q) of
        undefined -> undefined;
        _NewPos -> P
    end.

maybe_consume_prev(Pos, #mpd_state{consume = 1, queue = Q} = S) when Pos > 0 ->
    PrevPos = Pos - 1,
    S#mpd_state{queue = delete_at(PrevPos, Q)};
maybe_consume_prev(_, S) -> S.

%% ============================================================================
%% internal helpers — database
%% ============================================================================

%% fake music database
fake_db() ->
    [
     db_e(<<"The Midnight Echoes/First Light">>, [
          {<<"Title">>, <<"First Light">>},
          {<<"Artist">>, <<"The Midnight Echoes">>},
          {<<"Album">>, <<"First Light">>},
          {<<"Track">>, <<"1">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Indie Rock">>},
          {<<"duration">>, <<"245.0">>}
         ]),
     db_e(<<"The Midnight Echoes/First Light/Neon Rain">>, [
          {<<"Title">>, <<"Neon Rain">>},
          {<<"Artist">>, <<"The Midnight Echoes">>},
          {<<"Album">>, <<"First Light">>},
          {<<"Track">>, <<"2">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Indie Rock">>},
          {<<"duration">>, <<"198.0">>}
         ]),
     db_e(<<"The Midnight Echoes/First Light/Concrete Garden">>, [
          {<<"Title">>, <<"Concrete Garden">>},
          {<<"Artist">>, <<"The Midnight Echoes">>},
          {<<"Album">>, <<"First Light">>},
          {<<"Track">>, <<"3">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Indie Rock">>},
          {<<"duration">>, <<"312.0">>}
         ]),
     db_e(<<"The Midnight Echoes/Horizon Lines">>, [
          {<<"Title">>, <<"Horizon Lines">>},
          {<<"Artist">>, <<"The Midnight Echoes">>},
          {<<"Album">>, <<"Horizon Lines">>},
          {<<"Track">>, <<"1">>},
          {<<"Date">>, <<"2023">>},
          {<<"Genre">>, <<"Indie Rock">>},
          {<<"duration">>, <<"267.0">>}
         ]),
     db_e(<<"The Midnight Echoes/Horizon Lines/Paper Walls">>, [
          {<<"Title">>, <<"Paper Walls">>},
          {<<"Artist">>, <<"The Midnight Echoes">>},
          {<<"Album">>, <<"Horizon Lines">>},
          {<<"Track">>, <<"2">>},
          {<<"Date">>, <<"2023">>},
          {<<"Genre">>, <<"Indie Rock">>},
          {<<"duration">>, <<"223.0">>}
         ]),
     db_e(<<"Lena Sørensen/Parallel">>, [
          {<<"Title">>, <<"Parallel">>},
          {<<"Artist">>, <<"Lena Sørensen">>},
          {<<"Album">>, <<"Parallel">>},
          {<<"Track">>, <<"1">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Electronic">>},
          {<<"duration">>, <<"356.0">>}
         ]),
     db_e(<<"Lena Sørensen/Parallel/The Long Way Down">>, [
          {<<"Title">>, <<"The Long Way Down">>},
          {<<"Artist">>, <<"Lena Sørensen">>},
          {<<"Album">>, <<"Parallel">>},
          {<<"Track">>, <<"2">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Electronic">>},
          {<<"duration">>, <<"289.0">>}
         ]),
     db_e(<<"Lena Sørensen/Parallel/Static Bloom">>, [
          {<<"Title">>, <<"Static Bloom">>},
          {<<"Artist">>, <<"Lena Sørensen">>},
          {<<"Album">>, <<"Parallel">>},
          {<<"Track">>, <<"3">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Electronic">>},
          {<<"duration">>, <<"412.0">>}
         ]),
     db_e(<<"Lena Sørensen/Parallel/Signal Fade">>, [
          {<<"Title">>, <<"Signal Fade">>},
          {<<"Artist">>, <<"Lena Sørensen">>},
          {<<"Album">>, <<"Parallel">>},
          {<<"Track">>, <<"4">>},
          {<<"Date">>, <<"2024">>},
          {<<"Genre">>, <<"Electronic">>},
          {<<"duration">>, <<"178.0">>}
         ]),
     db_e(<<"The Rust Brothers/Gravel & Gold">>, [
          {<<"Title">>, <<"Gravel & Gold">>},
          {<<"Artist">>, <<"The Rust Brothers">>},
          {<<"Album">>, <<"Gravel & Gold">>},
          {<<"Track">>, <<"1">>},
          {<<"Date">>, <<"2023">>},
          {<<"Genre">>, <<"Blues Rock">>},
          {<<"duration">>, <<"203.0">>}
         ]),
     db_e(<<"The Rust Brothers/Gravel & Gold/Dust Road">>, [
          {<<"Title">>, <<"Dust Road">>},
          {<<"Artist">>, <<"The Rust Brothers">>},
          {<<"Album">>, <<"Gravel & Gold">>},
          {<<"Track">>, <<"2">>},
          {<<"Date">>, <<"2023">>},
          {<<"Genre">>, <<"Blues Rock">>},
          {<<"duration">>, <<"278.0">>}
         ])
    ].

db_e(File, TagPairs) ->
    #db_entry{file = File, tags = [{K, V} || {K, V} <- TagPairs]}.

%% lookup a database entry by file path
lookup_db(Path, #mpd_state{db = DB}) ->
    case [E || E <- DB, E#db_entry.file =:= Path] of
        [Entry | _] -> {ok, Entry};
        [] -> not_found
    end.

%% top-level directories: the first path component of each file
db_top_dirs(#mpd_state{db = DB}) ->
    lists:usort([first_component(E#db_entry.file) || E <- DB]).

first_component(Path) ->
    [First | _] = binary:split(Path, <<"/">>),
    First.

%% list contents of a directory
db_list_dir(Path, #mpd_state{db = DB}) ->
    PLen = byte_size(Path),
    Children = [E || E <- DB,
                    byte_size(E#db_entry.file) > PLen + 1,
                    binary:longest_common_prefix([E#db_entry.file, <<Path/binary, "/">>]) =:= PLen + 1],
    case Children of
        [] -> not_found;
        _ ->
            %% determine if children are directories or files
            %% a child is a directory if any entry has a deeper path
            SubDirs = lists:usort([second_component(E#db_entry.file, Path) || E <- Children]),
            case is_file_level(Children, Path) of
                true ->
                    {files, Children};
                false ->
                    {dirs, SubDirs}
            end
    end.

second_component(Full, Prefix) ->
    Rest = binary:part(Full, byte_size(Prefix) + 1, byte_size(Full) - byte_size(Prefix) - 1),
    [Comp | _] = binary:split(Rest, <<"/">>),
    Comp.

is_file_level(Entries, Prefix) ->
    PLen = byte_size(Prefix),
    lists:all(fun(E) ->
        Rest = binary:part(E#db_entry.file, PLen + 1, byte_size(E#db_entry.file) - PLen - 1),
        binary:match(Rest, <<"/">>) =:= nomatch
    end, Entries).

%% list command
db_list(<<"artist">>, [], _GroupBy, #mpd_state{db = DB}) ->
    Artists = lists:usort([tag(<<"Artist">>, E) || E <- DB]),
    [{<<"Artist">>, A} || A <- Artists, A =/= <<>>];

db_list(<<"album">>, [], _GroupBy, #mpd_state{db = DB}) ->
    Albums = lists:usort([tag(<<"Album">>, E) || E <- DB]),
    [{<<"Album">>, A} || A <- Albums, A =/= <<>>];

db_list(<<"album">>, [{<<"artist">>, Artist}], _GroupBy, #mpd_state{db = DB}) ->
    Albums = lists:usort([tag(<<"Album">>, E) || E <- DB, tag(<<"Artist">>, E) =:= Artist]),
    [{<<"Album">>, A} || A <- Albums, A =/= <<>>];

db_list(<<"genre">>, [], _GroupBy, #mpd_state{db = DB}) ->
    Genres = lists:usort([tag(<<"Genre">>, E) || E <- DB]),
    [{<<"Genre">>, G} || G <- Genres, G =/= <<>>];

db_list(_, _, _, _) ->
    [].

%% find: exact match on tag values
db_find(Exact, Filter, #mpd_state{db = DB}) ->
    Matches = db_match(Filter, DB),
    Matches2 = if Exact -> Matches; true -> Matches end,  %% same for now
    [db_entry_pairs(E) || E <- Matches2].

db_search(Exact, Filter, S) ->
    db_find(Exact, Filter, S).   %% same as find for now

%% match entries against a filter list
db_match([], DB) -> DB;
db_match([{Tag, Value} | Rest], DB) ->
    Matched = [E || E <- DB, tag_matches(Tag, Value, E)],
    db_match(Rest, Matched).

tag_matches(<<"any">>, Value, #db_entry{tags = Tags}) ->
    lists:any(fun({_K, V}) -> string:lowercase(V) =:= string:lowercase(Value) end, Tags);
tag_matches(Tag, Value, #db_entry{tags = Tags}) ->
    case lists:keyfind(Tag, 1, Tags) of
        {Tag, Value} -> true;
        _ -> false
    end.

%% helpers
tag(Key, #db_entry{tags = Tags}) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, V} -> V;
        false -> <<>>
    end.

tag_int(Key, Tags, Default) ->
    case lists:keyfind(Key, 1, Tags) of
        {Key, V} ->
            try binary_to_float(V) catch _:_ -> Default end;
        false -> Default
    end.

db_total_duration(#mpd_state{db = DB}) ->
    round(lists:sum([tag_int(<<"duration">>, E#db_entry.tags, 0.0) || E <- DB])).

db_distinct(Tag, #mpd_state{db = DB}) ->
    length(lists:usort([tag(Tag, E) || E <- DB, tag(Tag, E) =/= <<>>])).
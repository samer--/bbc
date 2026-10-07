%%% mpd_state - gen_server owning all mutable state.
%%%
%%% This is the heart of the actor design: instead of a mutex around global
%%% predicates, all state lives in ONE process.  Reads and writes are both
%%% messages to this process, serialised by the Erlang VM's message queue.
%%%
%%% The state record holds everything the Prolog version spread across
%%% state/2, vstate/2, and version_queue/2.

-module(mpd_state).
-behaviour(gen_server).

%% API
-export([start_link/0,
         status/0, stats/0,
         play/0, play/1, stop/0, pause/0, next/0, previous/0,
         add/1, delete/1, clear/0, playlistinfo/0,
         setvol/1, set_single/1, set_consume/1,
         subscribe/1, unsubscribe/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% --- records ---
-record(song, {file    :: string(),
               url     :: string(),
               id      :: pos_integer(),
               tags    :: [{string(), string()}],
               pos     :: integer() | undefined}).

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
          error          = undefined :: undefined | string(),
          subscribers    = []     :: [pid()],
          start_time     = 0      :: integer()
         }).

%% --- API functions ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% reading state
status()        -> gen_server:call(?MODULE, status).
stats()         -> gen_server:call(?MODULE, stats).
playlistinfo()  -> gen_server:call(?MODULE, playlistinfo).

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

%% --- gen_server callbacks ---

init([]) ->
    {ok, #mpd_state{start_time = erlang:system_time(second)}}.

handle_call(status, _From, S) ->
    {reply, {ok, status_reply(S)}, S};

handle_call(stats, _From, S) ->
    Now = erlang:system_time(second),
    Stats = [
        {<<"uptime">>, Now - S#mpd_state.start_time},
        {<<"songs">>, length(S#mpd_state.queue)},
        {<<"playlist">>, S#mpd_state.queue_version},
        {<<"db_playtime">>, 0}
    ],
    {reply, {ok, Stats}, S};

handle_call(playlistinfo, _From, S) ->
    Songs = [song_info(I, Song) || {I, Song} <- lists:zip(lists:seq(0, length(S#mpd_state.queue)-1), S#mpd_state.queue)],
    {reply, {ok, Songs}, S};

%% playback
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

%% queue mutation
handle_call({add, Path}, _From, S) ->
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
    {reply, {ok, Id}, S#mpd_state{queue = NewQ, queue_version = Ver, next_id = Id+1}};

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

%% options
handle_call({setvol, V}, _From, S) when V >= 0, V =< 100 ->
    mpd_player:set_volume(V),
    notify_subscribers(<<"mixer">>, S),
    {reply, ok, S#mpd_state{volume = V}};

handle_call({setvol, _}, _From, S) ->
    {reply, {error, <<"volume out of range">>}, S};

handle_call({single, V}, _From, S) when V =:= 0; V =:= 1 ->
    {reply, ok, S#mpd_state{single = V}};

handle_call({consume, V}, _From, S) when V =:= 0; V =:= 1 ->
    {reply, ok, S#mpd_state{consume = V}}.

handle_cast({subscribe, Pid}, S) ->
    NewSubs = [Pid | lists:delete(Pid, S#mpd_state.subscribers)],
    {noreply, S#mpd_state{subscribers = NewSubs}};

handle_cast({unsubscribe, Pid}, S) ->
    {noreply, S#mpd_state{subscribers = lists:delete(Pid, S#mpd_state.subscribers)}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
    %% subscriber died - remove them
    {noreply, S#mpd_state{subscribers = lists:delete(Pid, S#mpd_state.subscribers)}};

handle_info(_Msg, S) ->
    {noreply, S}.

%% --- internal helpers ---

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

position_of(Id, Q) ->
    position_of(Id, Q, 0).

position_of(Id, [#song{id = Id} | _], Pos) -> Pos;
position_of(Id, [_ | Rest], Pos) -> position_of(Id, Rest, Pos + 1);
position_of(_, [], _) -> undefined.

delete_at(Pos, Q) ->
    {Front, [_ | Back]} = lists:split(Pos, Q),
    Front ++ Back.

adjust_player_delete(_Pos, undefined, _) -> undefined;
adjust_player_delete(_Pos, #player{song_id = Id} = P, Q) ->
    case position_of(Id, Q) of
        undefined -> undefined;  %% deleted the current song
        _NewPos -> P
    end.

maybe_consume_prev(Pos, #mpd_state{consume = 1, queue = Q} = S) when Pos > 0 ->
    PrevPos = Pos - 1,
    S#mpd_state{queue = delete_at(PrevPos, Q)};
maybe_consume_prev(_, S) -> S.
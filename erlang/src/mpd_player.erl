%%% mpd_player - Player stub (gen_server).
%%%
%%% In the real system this wraps a GStreamer subprocess via a port.
%%% Here it just logs commands — enough to verify the actor wiring.

-module(mpd_player).
-behaviour(gen_server).

-export([start_link/0, play/1, stop/0, pause/0, set_volume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {current :: undefined | pos_integer(), volume :: integer()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

play(Id) -> gen_server:call(?MODULE, {play, Id}).
stop()   -> gen_server:cast(?MODULE, stop).
pause()  -> gen_server:cast(?MODULE, pause).
set_volume(V) -> gen_server:cast(?MODULE, {set_volume, V}).

init([]) ->
    io:format("~p [~p] started~n", [?MODULE, self()]),
    {ok, #state{current = undefined, volume = 50}}.

handle_call({play, Id}, _From, #state{current = _Cur} = S) ->
    io:format("~p [~p] PLAY  song_id=~B~n", [?MODULE, self(), Id]),
    {reply, ok, S#state{current = Id}}.

handle_cast(stop, #state{current = _Cur} = S) ->
    io:format("~p [~p] STOP~n", [?MODULE, self()]),
    {noreply, S#state{current = undefined}};

handle_cast(pause, #state{current = _Cur} = S) ->
    io:format("~p [~p] PAUSE~n", [?MODULE, self()]),
    {noreply, S};

handle_cast({set_volume, V}, S) ->
    io:format("~p [~p] VOLUME ~B~n", [?MODULE, self(), V]),
    {noreply, S#state{volume = V}}.
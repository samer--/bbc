%%% mpd_sup - Top-level supervisor.
%%%
%%% Supervision tree:
%%%
%%%   mpd_sup (one_for_one)
%%%   ├── mpd_state      (worker, permanent)
%%%   ├── mpd_player     (worker, permanent)
%%%   ├── mpd_acceptor   (worker, permanent)
%%%   └── mpd_client_sup (supervisor, simple_one_for_one)
%%%         └── mpd_client  (temporary, one per TCP connection)

-module(mpd_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},

    State = #{id => mpd_state,
              start => {mpd_state, start_link, []},
              restart => permanent,
              shutdown => 5000,
              type => worker,
              modules => [mpd_state]},

    Player = #{id => mpd_player,
               start => {mpd_player, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [mpd_player]},

    Acceptor = #{id => mpd_acceptor,
                 start => {mpd_acceptor, start_link, []},
                 restart => permanent,
                 shutdown => 5000,
                 type => worker,
                 modules => [mpd_acceptor]},

    ClientSup = #{id => mpd_client_sup,
                  start => {mpd_client_sup, start_link, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => supervisor,
                  modules => [mpd_client_sup]},

    {ok, {SupFlags, [State, Player, Acceptor, ClientSup]}}.
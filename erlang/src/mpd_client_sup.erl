%%% mpd_client_sup - Simple-one-for-one supervisor for mpd_client processes.

-module(mpd_client_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    Child = #{id => mpd_client,
              start => {mpd_client, start_link, []},
              restart => temporary,
              shutdown => 5000,
              type => worker,
              modules => [mpd_client]},
    {ok, {#{strategy => simple_one_for_one, intensity => 0, period => 1}, [Child]}}.
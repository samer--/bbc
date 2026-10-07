%%% mpd_app - Application callback module.

-module(mpd_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    mpd_sup:start_link().

stop(_State) ->
    ok.
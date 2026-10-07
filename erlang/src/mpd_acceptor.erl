%%% mpd_acceptor - TCP listener (gen_server).
%%%
%%% Listens on the configured port, spawns an mpd_client per connection.

-module(mpd_acceptor).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {socket :: inet:socket()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = application:get_env(mpd, port, 6600),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}]),
    io:format("~p listening on port ~B~n", [?MODULE, Port]),
    %% trigger first accept
    self() ! accept,
    {ok, #state{socket = LSock}}.

handle_info(accept, #state{socket = LSock} = S) ->
    {ok, CSock} = gen_tcp:accept(LSock),
    io:format("~p accepted connection from ~p~n", [?MODULE, inet:peername(CSock)]),
    {ok, Pid} = mpd_client_sup:start_child(),
    gen_tcp:controlling_process(CSock, Pid),
    Pid ! {socket, CSock},
    self() ! accept,
    {noreply, S};

handle_info(_Msg, S) ->
    {noreply, S}.

handle_call(_Req, _From, S) -> {reply, ok, S}.
handle_cast(_Req, S) -> {noreply, S}.

terminate(_Reason, #state{socket = LSock}) ->
    gen_tcp:close(LSock).
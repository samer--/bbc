%%% mpd_client - per-connection MPD client handler (gen_statem).
%%%
%%% One process per TCP connection.  Parses incoming MPD commands,
%%% calls mpd_state for reads/mutations, formats and sends replies.

-module(mpd_client).
-behaviour(gen_statem).

-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0,
         waiting_socket/3, ready/3, idle/3,
         terminate/3, code_change/4]).

%% --- state record ---
-record(client, {
          socket  :: inet:socket() | undefined
         }).

%% --- API ---
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%% --- gen_statem ---
callback_mode() -> state_functions.

init([]) ->
    {ok, waiting_socket, #client{socket = undefined}}.

%% --- state: waiting for acceptor to hand over the socket ---

waiting_socket(info, {socket, Socket}, C) ->
    inet:setopts(Socket, [{active, once}, {packet, line}]),
    gen_tcp:send(Socket, <<"OK MPD 0.21.0\n">>),
    {next_state, ready, C#client{socket = Socket}}.

%% --- state: ready ---

ready(info, {tcp, _Sock, Data}, C) ->
    Socket = C#client.socket,
    inet:setopts(Socket, [{active, once}, {packet, line}]),
    handle_line(Data, Socket, C);

ready(info, {tcp_closed, _Sock}, _C) ->
    {stop, normal};

ready(info, {tcp_error, _Sock, _Reason}, _C) ->
    {stop, normal};

ready(info, {mpd_changed, _Subsystem}, C) ->
    {keep_state, C};

ready(_, _, C) ->
    {keep_state, C}.

%% --- state: idle ---

idle(info, {tcp, _Sock, Data}, C) ->
    Socket = C#client.socket,
    inet:setopts(Socket, [{active, once}, {packet, line}]),
    handle_line(Data, Socket, C);

idle(info, {tcp_closed, _Sock}, _C) ->
    {stop, normal};

idle(info, {tcp_error, _Sock, _Reason}, _C) ->
    {stop, normal};

idle(info, {mpd_changed, Subsystem}, C) ->
    Socket = C#client.socket,
    Line = iolist_to_binary(io_lib:format("changed: ~s\n", [Subsystem])),
    gen_tcp:send(Socket, Line),
    gen_tcp:send(Socket, <<"OK\n">>),
    {next_state, ready, C};

idle(_, _, C) ->
    {keep_state, C}.

terminate(_Reason, _State, #client{socket = Sock}) ->
    mpd_state:unsubscribe(self()),
    case Sock of
        undefined -> ok;
        _ -> gen_tcp:close(Sock)
    end.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

%% --- command dispatch ---
%% handle_line returns a gen_statem transition so the caller can forward it

handle_line(<<"idle">>, _Socket, _C) ->
    mpd_state:subscribe(self()),
    {next_state, idle};

handle_line(<<"noidle">>, Socket, C) ->
    gen_tcp:send(Socket, <<"OK\n">>),
    {keep_state, C};

handle_line(<<"close">>, _Socket, _C) ->
    {stop, normal};

handle_line(Line, Socket, C) ->
    case mpd_protocol:parse(Line) of
        {ok, {Cmd, Args}} ->
            Reply = dispatch(Cmd, Args),
            gen_tcp:send(Socket, Reply);
        {error, _} ->
            gen_tcp:send(Socket, mpd_protocol:format_error(99, <<"parse error">>))
    end,
    {keep_state, C}.

%% command dispatch table

dispatch(<<"commands">>, _) ->
    Cmds = [<<"close">>, <<"commands">>, <<"status">>, <<"stats">>,
            <<"play">>, <<"stop">>, <<"pause">>, <<"next">>, <<"previous">>,
            <<"add">>, <<"delete">>, <<"clear">>,
            <<"playlistinfo">>, <<"setvol">>,
            <<"single">>, <<"consume">>, <<"idle">>, <<"noidle">>,
            <<"outputs">>, <<"tagtypes">>, <<"decoders">>, <<"ping">>],
    Pairs = [{<<"command">>, C} || C <- Cmds],
    mpd_protocol:format_reply(Pairs);

dispatch(<<"status">>, _) ->
    case mpd_state:status() of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, _}  -> mpd_protocol:format_error(99, <<"status failed">>)
    end;

dispatch(<<"stats">>, _) ->
    case mpd_state:stats() of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, _}  -> mpd_protocol:format_error(99, <<"stats failed">>)
    end;

dispatch(<<"play">>, []) ->
    case mpd_state:play() of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(2, Msg)
    end;

dispatch(<<"play">>, [PosBin]) ->
    Pos = binary_to_integer(PosBin),
    case mpd_state:play(Pos) of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(2, Msg)
    end;

dispatch(<<"stop">>, _) ->
    mpd_state:stop(),
    mpd_protocol:format_ok();

dispatch(<<"pause">>, _) ->
    case mpd_state:pause() of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(2, Msg)
    end;

dispatch(<<"next">>, _) ->
    mpd_state:next(),
    mpd_protocol:format_ok();

dispatch(<<"previous">>, _) ->
    case mpd_state:previous() of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(2, Msg)
    end;

dispatch(<<"add">>, [Path]) ->
    case mpd_state:add(Path) of
        {ok, _Id} -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"delete">>, [PosBin]) ->
    Pos = binary_to_integer(PosBin),
    case mpd_state:delete(Pos) of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(50, Msg)
    end;

dispatch(<<"clear">>, _) ->
    mpd_state:clear(),
    mpd_protocol:format_ok();

dispatch(<<"playlistinfo">>, _) ->
    case mpd_state:playlistinfo() of
        {ok, Songs} ->
            AllPairs = lists:append(Songs),
            mpd_protocol:format_reply(AllPairs);
        {error, _} -> mpd_protocol:format_error(99, <<"playlistinfo failed">>)
    end;

dispatch(<<"setvol">>, [VBin]) ->
    V = binary_to_integer(VBin),
    case mpd_state:setvol(V) of
        ok -> mpd_protocol:format_ok();
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"single">>, [VBin]) ->
    V = binary_to_integer(VBin),
    mpd_state:set_single(V),
    mpd_protocol:format_ok();

dispatch(<<"consume">>, [VBin]) ->
    V = binary_to_integer(VBin),
    mpd_state:set_consume(V),
    mpd_protocol:format_ok();

dispatch(<<"outputs">>, _) ->
    mpd_protocol:format_reply([
        {<<"outputid">>, 0},
        {<<"outputname">>, <<"Default output">>},
        {<<"outputenabled">>, 1}
    ]);

dispatch(<<"tagtypes">>, _) ->
    mpd_protocol:format_reply([
        {<<"tagtype">>, <<"Artist">>},
        {<<"tagtype">>, <<"Album">>},
        {<<"tagtype">>, <<"Title">>},
        {<<"tagtype">>, <<"Track">>},
        {<<"tagtype">>, <<"Date">>},
        {<<"tagtype">>, <<"Comment">>}
    ]);

dispatch(<<"decoders">>, _) ->
    mpd_protocol:format_ok();

dispatch(<<"ping">>, _) ->
    mpd_protocol:format_ok();

dispatch(Cmd, _Args) ->
    Msg = iolist_to_binary(io_lib:format("unknown command ~s", [Cmd])),
    mpd_protocol:format_error(5, Msg).
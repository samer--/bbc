%%% mpd_client - per-connection MPD client handler (gen_statem).
%%%
%%% One process per TCP connection.  Parses incoming MPD commands,
%%% calls mpd_state for reads/mutations, formats and sends replies.
%%%
%%% Supports command lists (command_list_begin / command_list_end and
%%% command_list_ok_begin / command_list_end) for batch command execution.

-module(mpd_client).
-behaviour(gen_statem).

-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0,
         waiting_socket/3, ready/3, idle/3, command_list/3,
         terminate/3, code_change/4]).

%% --- state record ---
-record(client, {
          socket  :: inet:socket() | undefined,
          pending = [] :: [{binary(), [binary()]}],  %% accumulated commands for command_list
          sub_reply = silent :: silent | list_ok     %% command_list reply mode
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
    gen_tcp:send(Socket, <<"OK MPD 0.21.26\n">>),
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

%% --- state: command_list ---
%% Accumulate commands; execute on command_list_end.

command_list(info, {tcp, _Sock, Data}, C) ->
    Socket = C#client.socket,
    inet:setopts(Socket, [{active, once}, {packet, line}]),
    handle_clist_line(Data, Socket, C);

command_list(info, {tcp_closed, _Sock}, _C) ->
    {stop, normal};

command_list(info, {tcp_error, _Sock, _Reason}, _C) ->
    {stop, normal};

command_list(_, _, C) ->
    {keep_state, C}.

terminate(_Reason, _State, #client{socket = Sock}) ->
    mpd_state:unsubscribe(self()),
    case Sock of
        undefined -> ok;
        _ -> gen_tcp:close(Sock)
    end.

code_change(_Vsn, State, Data, _Extra) -> {ok, State, Data}.

%% ============================================================================
%% line handlers
%% ============================================================================

%% --- ready/idle: handle a single command ---

handle_line(Line0, Socket, C) ->
    Line = chomp(Line0),
    handle_chomped(Line, Socket, C).

handle_chomped(<<"command_list_ok_begin">>, _Socket, C) ->
    {next_state, command_list, C#client{pending = [], sub_reply = list_ok}};

handle_chomped(<<"command_list_begin">>, _Socket, C) ->
    {next_state, command_list, C#client{pending = [], sub_reply = silent}};

handle_chomped(<<"idle">>, _Socket, _C) ->
    mpd_state:subscribe(self()),
    {next_state, idle};

handle_chomped(<<"noidle">>, Socket, C) ->
    gen_tcp:send(Socket, <<"OK\n">>),
    {keep_state, C};

handle_chomped(<<"close">>, _Socket, _C) ->
    {stop, normal};

handle_chomped(Line, Socket, C) ->
    case mpd_protocol:parse(Line) of
        {ok, {Cmd, Args}} ->
            Reply = dispatch(Cmd, Args),
            gen_tcp:send(Socket, Reply);
        {error, _} ->
            gen_tcp:send(Socket, mpd_protocol:format_error(99, <<"parse error">>))
    end,
    {keep_state, C}.

%% --- command_list: accumulate or execute ---

handle_clist_line(Line0, Socket, C) ->
    Line = chomp(Line0),
    handle_clist_chomped(Line, Socket, C).

handle_clist_chomped(<<"command_list_end">>, Socket, C) ->
    %% execute all accumulated commands
    lists:foreach(
        fun({Cmd, Args}) ->
            Reply0 = dispatch(Cmd, Args),
            Reply = strip_trailing_ok(Reply0),
            gen_tcp:send(Socket, Reply),
            case C#client.sub_reply of
                list_ok -> gen_tcp:send(Socket, <<"list_OK\n">>);
                silent -> ok
            end
        end,
        lists:reverse(C#client.pending)),
    gen_tcp:send(Socket, <<"OK\n">>),
    {next_state, ready, C#client{pending = [], sub_reply = silent}};

handle_clist_chomped(Line, _Socket, C) ->
    case mpd_protocol:parse(Line) of
        {ok, {Cmd, Args}} ->
            {keep_state, C#client{pending = [{Cmd, Args} | C#client.pending]}};
        {error, _} ->
            {keep_state, C}
    end.

%% ============================================================================
%% command dispatch table
%% ============================================================================

dispatch(<<"commands">>, _) ->
    Cmds = [<<"close">>, <<"commands">>, <<"status">>, <<"stats">>,
            <<"play">>, <<"stop">>, <<"pause">>, <<"next">>, <<"previous">>,
            <<"add">>, <<"delete">>, <<"clear">>,
            <<"playlistinfo">>, <<"currentsong">>, <<"setvol">>,
            <<"single">>, <<"consume">>, <<"idle">>, <<"noidle">>,
            <<"outputs">>, <<"tagtypes">>, <<"decoders">>, <<"ping">>,
            <<"lsinfo">>, <<"list">>, <<"find">>, <<"search">>, <<"count">>,
            <<"update">>, <<"command_list_ok_begin">>, <<"command_list_begin">>,
            <<"command_list_end">>],
    Pairs = [{<<"command">>, C} || C <- Cmds],
    mpd_protocol:format_reply(Pairs);

dispatch(<<"currentsong">>, _) ->
    case mpd_state:currentsong() of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, _} -> mpd_protocol:format_ok()
    end;

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
        {<<"tagtype">>, <<"Genre">>},
        {<<"tagtype">>, <<"Comment">>}
    ]);

dispatch(<<"decoders">>, _) ->
    mpd_protocol:format_ok();

dispatch(<<"ping">>, _) ->
    mpd_protocol:format_ok();

%% --- database commands ---

dispatch(<<"lsinfo">>, [Path]) ->
    case mpd_state:lsinfo(Path) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"lsinfo">>, []) ->
    case mpd_state:lsinfo(<<>>) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"list">>, Args) ->
    {Type, Filter, GroupBy} = parse_list_args(Args),
    case mpd_state:list(Type, Filter, GroupBy) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"find">>, Args) ->
    Filter = parse_filter(Args),
    case mpd_state:find(true, Filter) of
        {ok, Songs} ->
            AllPairs = lists:append(Songs),
            mpd_protocol:format_reply(AllPairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"search">>, Args) ->
    Filter = parse_filter(Args),
    case mpd_state:search(false, Filter) of
        {ok, Songs} ->
            AllPairs = lists:append(Songs),
            mpd_protocol:format_reply(AllPairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"count">>, Args) ->
    Filter = parse_filter(Args),
    case mpd_state:count(Filter) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"update">>, [Path]) ->
    case mpd_state:update(Path) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(<<"update">>, []) ->
    case mpd_state:update(<<>>) of
        {ok, Pairs} -> mpd_protocol:format_reply(Pairs);
        {error, Msg} -> mpd_protocol:format_error(99, Msg)
    end;

dispatch(Cmd, _Args) ->
    Msg = iolist_to_binary(io_lib:format("unknown command ~s", [Cmd])),
    mpd_protocol:format_error(5, Msg).

%% ============================================================================
%% argument parsers for list / find / search / count
%% ============================================================================

%% parse list command args: list <type> [<tag> <value>] [group <tag>]
parse_list_args([TypeBin | Rest]) ->
    Type = string:lowercase(binary_to_list(TypeBin)),
    {Filter, GroupBy} = split_filter_group(Rest),
    {list_to_binary(Type), Filter, GroupBy}.

%% parse find/search/count args: <tag> <value> [<tag> <value> ...]
parse_filter(Args) ->
    parse_filter_pairs(Args, []).

parse_filter_pairs([], Acc) -> lists:reverse(Acc);
parse_filter_pairs([Tag, Value | Rest], Acc) ->
    TagBin = iolist_to_binary(string:lowercase(binary_to_list(Tag))),
    parse_filter_pairs(Rest, [{TagBin, Value} | Acc]);
parse_filter_pairs([_], Acc) ->
    lists:reverse(Acc).

split_filter_group(Args) ->
    case lists:splitwith(fun(X) -> X =/= <<"group">> end, Args) of
        {FilterPairs, []} ->
            {parse_filter(FilterPairs), []};
        {FilterPairs, [<<"group">> | Groups]} ->
            {parse_filter(FilterPairs), Groups}
    end.

%% strip trailing newline / carriage return
chomp(B) when is_binary(B) ->
    S = byte_size(B),
    if S =:= 0 -> B;
       true -> chomp_last(B, S)
    end.

chomp_last(B, S) ->
    case binary:at(B, S - 1) of
        X when X =:= $\n; X =:= $\r ->
            chomp(binary:part(B, 0, S - 1));
        _ -> B
    end.

%% strip trailing "OK\n" from a response iolist
strip_trailing_ok(Reply) ->
    Bin = iolist_to_binary(Reply),
    Size = byte_size(Bin),
    case Bin of
        <<"OK\n">> -> <<>>;
        _ when Size >= 3 ->
            case binary:part(Bin, Size - 3, 3) of
                <<"OK\n">> -> binary:part(Bin, 0, Size - 3);
                _ -> Bin
            end;
        _ -> Bin
    end.
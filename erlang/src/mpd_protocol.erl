%%% mpd_protocol - MPD text protocol parsing.
%%%
%%% Parses MPD command lines like:
%%%   command arg1 "arg2 with spaces"
%%% Into:
%%%   {command, [Arg]}
%%%
%%% Each Arg is a binary.  Quoted args have quotes stripped.
%%% Bare args are unquoted whitespace-delimited tokens.

-module(mpd_protocol).
-export([parse/1, format_reply/1, format_ok/0, format_error/2]).

%% parse(Line :: binary()) -> {ok, {Command :: binary(), Args :: [binary()]}}
%%                          | {error, Reason}
parse(Line0) ->
    Line = trim(Line0),
    case binary:split(Line, <<" ">>) of
        [Cmd] ->
            {ok, {lower(Cmd), []}};
        [Cmd, Rest] ->
            case parse_args(Rest) of
                {ok, Args} -> {ok, {lower(Cmd), Args}};
                {error, _} = E -> E
            end
    end.

%% format_reply/1: Format a list of {Key, Value} pairs as MPD response lines
%% Returns an iolist ready for gen_tcp:send, ending with "OK\n"
format_reply(Pairs) ->
    [format_result(Pairs), <<"OK\n">>].

%% format_result/1: Format pairs WITHOUT trailing OK (for command lists)
format_result([]) -> [];
format_result(Pairs) ->
    [[K, <<": ">>, val(V), <<"\n">>] || {K, V} <- Pairs].

format_ok() ->
    <<"OK\n">>.

format_error(Code, Msg) ->
    Ack = io_lib:format("ACK [~B@0] {} ~s\n", [Code, Msg]),
    iolist_to_binary(Ack).

%% --- internal ---

trim(B) -> re:replace(B, <<"(^\\s+|\\s+$)">>, <<"">>, [global, {return, binary}]).

lower(B) ->
    list_to_binary(string:lowercase(binary_to_list(B))).

parse_args(Bin) -> parse_args(Bin, []).

parse_args(<<>>, Acc) -> {ok, lists:reverse(Acc)};
parse_args(<<" ", Rest/binary>>, Acc) -> parse_args(Rest, Acc);
parse_args(<<"\"", Rest/binary>>, Acc) ->
    case binary:split(Rest, <<"\"">>) of
        [Arg, Tail] -> parse_args(Tail, [Arg | Acc]);
        [_] -> {error, unterminated_quote}
    end;
parse_args(Rest, Acc) ->
    case binary:split(Rest, <<" ">>) of
        [Arg, Tail] -> parse_args(Tail, [Arg | Acc]);
        [Arg] -> {ok, lists:reverse([Arg | Acc])}
    end.

val(V) when is_binary(V) -> V;
val(V) when is_integer(V) -> integer_to_binary(V);
val(V) when is_float(V) -> float_to_binary(V, [{decimals, 3}]);
val(V) when is_list(V) -> list_to_binary(V).
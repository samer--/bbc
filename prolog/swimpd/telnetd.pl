:- module(telnetd, [telnet_server/3]).

:- use_module(library(socket)).
:- use_module(tools, [spawn/1]).

%!  telnet_server(+Interactor, +Port, +Options) is det.
%   Interactor is a callable goal that implements a protocl on the current
%   Prolog input and output streams.  Defined options are:
%
%           * allow(pred(ip))
%           Allow access from addresses that satisfy unary predicate over ip, where
%           =|ip ---> ip(integer, integer, integer, integer)|=.
%           Default is =|=(ip(127,0,0,1]))|= (i.e. localhost only).
telnet_server(P, Port, Options) :-
   option(allow(Allow), Options, =(ip(127,0,0,1))),
   setup_call_cleanup(tcp_socket(Socket),
                      socket_server(P, Socket, Port, Allow),
                      tcp_close_socket(Socket)).

socket_server(P, Socket, Port, Allow) :-
   tcp_setopt(Socket, reuseaddr),
   tcp_bind(Socket, Port),
   tcp_listen(Socket, 5),
   server_loop(P, Socket, Allow).

server_loop(P, Socket, Allow) :-
   tcp_accept(Socket, Slave, Peer),
   debug(mpd(connection), "new connection from ~w", [Peer]),
   tcp_open_socket(Slave, IO),
   spawn(call_cleanup(client_thread(P, IO, Peer, Allow),
                      (debug(mpd(connection), 'Closing connection from ~w', [Peer]), close(IO)))),
   server_loop(P, Socket, Allow).

client_thread(P, IO, Peer, Allow) :- call(Allow, Peer), !, service_client(P, IO).
client_thread(_, IO, _, _) :- format(IO, 'Access denied.~n', []).

service_client(P, IO) :-
   maplist(set_stream(IO), [close_on_abort(false), encoding(utf8), newline(posix), buffer(full)]),
   stream_pair(IO, In, Out), set_input(In), set_output(Out), call(P).

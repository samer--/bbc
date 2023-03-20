:- module(telnetd, [telnet_server/3]).

:- use_module(library(socket)).

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
   debug(swimpd(telnet, s(s(0))), 'Telnet server running on port ~w', [Port]),
   catch(server_loop(P, Socket, Allow), shutdown,
         debug(swimpd(telnet, s(s(0))), 'Shutting down telnet server on port ~w', [Port])).

server_loop(P, Socket, Allow) :-
   tcp_accept(Socket, Slave, Peer),
   debug(swimpd(telnet, s(0)), "new connection from ~w", [Peer]),
   tcp_open_socket(Slave, IO),
   thread_create(client(P, IO, Peer, Allow), _, [at_exit(close_peer(Peer, IO)), detached(true)]),
   server_loop(P, Socket, Allow).

close_peer(Peer, IO) :- debug(swimpd(telnet, s(0)), 'Closing connection from ~w', [Peer]), close(IO).
client(P, IO, Peer, Allow) :- call(Allow, Peer), !, service_client(P, IO).
client(_, IO, _, _) :- format(IO, 'Access denied.~n', []).

service_client(P, IO) :-
   thread_self(Self), debug(swimpd(telnet, s(0)), 'Servicing telnet client on thread ~w', [Self]),
   maplist(set_stream(IO), [close_on_abort(false), encoding(utf8), newline(posix), buffer(full)]),
   stream_pair(IO, In, Out), set_input(In), set_output(Out), call(P).

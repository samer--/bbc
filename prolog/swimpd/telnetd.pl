:- module(telnetd, [telnet_server/3]).

:- use_module(library(socket)).
:- use_module(tools, [tracing_death/1]).

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
   debug(mpd(telnet, s(s(0))), 'Telnet server running on port ~w', [Port]),
   catch(server_loop(P, Socket, Allow), shutdown,
         debug(mpd(telnet, s(s(0))), 'Shutting down telnet server on port ~w', [Port])).

:- det(server_loop/3).
server_loop(P, Socket, Allow) :-
   tcp_accept(Socket, Slave, Peer),
   debug(mpd(telnet, s(0)), "new connection from ~w", [Peer]),
   tcp_open_socket(Slave, IO),
   thread_create(client(P, IO, Peer, Allow), _, [at_exit(close_peer(Peer, IO)), detached(true)]),
   server_loop(P, Socket, Allow).

close_peer(Peer, IO) :- debug(mpd(telnet, s(0)), 'Closing connection from ~w', [Peer]), close(IO).
client(P, IO, Peer, Allow) :- call(Allow, Peer), !, service_client(P, IO).
client(_, IO, _, _) :- format(IO, 'Access denied.~n', []).

service_client(P, IO) :-
   thread_self(Self), debug(mpd(telnet, s(0)), 'Servicing telnet client on thread ~w', [Self]),
   maplist(set_stream(IO), [close_on_abort(false), encoding(utf8), newline(posix), buffer(full)]),
   stream_pair(IO, In, Out), set_input(In), set_output(Out),
   setup_call_catcher_cleanup(true, P, Status, debug(mpd(telnet, s(0)), "Client termination status: ~w", [Status])).

:- module(telnetd, [telnet_server/3]).

:- use_module(library(socket)).
:- use_module(asyncu, [spawn/1, setup_stream/2]).

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
   tcp_open_socket(Slave, In, Out),
   maplist(setup_stream([close_on_abort(false)]), [In, Out]),
   catch(spawn(call_cleanup(client_thread(P, In, Out, Peer, Allow), (close(In), close(Out)))),
         error(permission_error(create, thread, mpdclient), _), fail), !,
   server_loop(P, Socket, Allow).

client_thread(P, In, Out, Peer, Allow) :- call(Allow, Peer), !, service_client(P, In, Out).
client_thread(_, _, Out, _, _) :- format(Out, 'Access denied.~n', []).

service_client(P, In, Out) :-
   set_prolog_IO(In, Out, user_error), prompt(_, ''),
   current_prolog_flag(encoding, Enc),
   maplist(setup_stream([encoding(Enc), newline(posix)]), [user_input, user_output]),
   call(P).

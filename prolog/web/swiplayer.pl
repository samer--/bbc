#!/usr/local/bin/swipl -g run(prolog)

:- module(swiplayer,[run/1, run/0]).
/** <module> BBC iPlayer via Prolog web server

   This is going to be great.
*/
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_cache)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_portray)).

:- use_module(library(hostname)).
:- use_module(library(dcg_core)).
:- use_module(library(plweb/webutils)).
:- use_module(library(plweb/htmlutils)).
:- use_module(framework(basic)).
:- use_module(framework(bootstrap)).
:- use_module(framework(pure), []).
:- use_module(framework(kube), []).

:- set_prolog_flag(double_quote,string).

:- rdf_set_cache_options([enabled(true),global_directory(cache)]).
:- setting(port, integer, 8228, "Listening port for server").
:- setting(style, ground, pure(2,swiplayer:nav), "CSS framework and page style").

:- set_setting(htmlutils:appname,"swiPlayer").

http:location(alt, root(alt), []).

:- http_handler(root(.), root, []).
:- http_handler(root(hello), hello, []).
:- http_handler(alt(hello), reply_html(_,stdpage("Hello page",hello)), [id(alt(hello))]).
:- http_handler(alt(home), reply_html(_,stdpage("Root",root)), []).
:- http_handler(root(home), root, [id(home)]).
:- http_handler(root(search), hello, [id(search)]).
:- http_handler(root(cats), hello, [id(cats)]).
:- http_handler(root(channels), hello, [id(channels)]).

:- googlefont(sourcepro,"Source+Sans+Pro|Source+Code+Pro").
:- googlefont(opensans,"Open+Sans").
:- googlefont(quattrocento,"Quattrocento|Quattrocento+Sans").

root(_) :- 
	hostname(Hostname),
	setting(style, Style),
	reply_html_page( Style,
     [ title("swiPlayer home") ],
     [ h1("swiPlayer (on ~w)"-[Hostname]),
       \html_requires("//maxcdn.bootstrapcdn.com/font-awesome/4.1.0/css/font-awesome.min.css"),
       p([ "Hello! ", 
           \button_link(hello, [i(class('fa fa-play'),[]), " Hello page"])
         ])]).

hello(_) :- 
	setting(style, Style),
	reply_html_page( Style,
	                 [ title("swiPlayer home") ],
						  [ h1("Hello page"),
						    p(["Hello! ",b(hello),"."]) ]).

nav --> html(ul([li(\link(home,"swiPlayer")), \navitems])).

navitems --> html( [ li(\link(cats,"Categories"))
              , li(\link(channels,"Channels"))
              , li(\link(search,"Search")) 
              ]).

stdpage(Heading,Body) --> 
   {hostname(Hostname)},
   {format(atom(Title),"Test server on ~w - ~w",[Hostname,Heading])},
   {setting(style,Style)},
	page(Style,[title(Title)],[h1(Heading),\Body]).

hello --> html([p(["Hello! ",b(hello),"."]), \rhubarb(10,30)]).
root --> html([p("Hello and welcome!"), \link(alt(hello),"Hello page")]).

run :- 
   setting(port,Port), 
   start_server(swiplayer,Port).

run(Goal) :- 
   setting(port,Port), 
   with_server(swiplayer,Port,Goal).

load :-
  rdf_load("http://www.w3.org/2004/02/skos/core").

rhubarb(N,M) --> rep(N, html(p(["Rhubarb", \rep(M,html(" rhubarb")), "."]))).

button_link(A,B) -->
   { setting(style,Style), functor(Style,Func,_)},
   call(Func:button_link(A,B)).

#!/usr/bin/env swipl

/** <module> Server

@author Tibo Vanheule

*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(parser).
:- use_module(write).
:- use_module(ai).
:- use_module(game).

:- initialization(http_daemon, main).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders,'/hex/src/website/').

:- http_handler(root(.), http_reply_from_files('./website', []), [prefix]).
:- http_handler(files(.), serve_files_in_directory(folders), [prefix]).
:- http_handler('/move', move_handler, []).
:- http_handler('/move/svg', svg_handler, []).

move_handler(Request) :-
  member(method(post), Request), !,
  http_read_data(Request, Data, []),
  open_string(Data, Stream),
  parse_server(Stream, GameBoard),
  bestmove(GameBoard,3,Move),
  update_board(GameBoard,Move,NewBoard),
  json_write_board(NewBoard, Json),
  reply_json_dict(Json).
move_handler(Request) :-
  \+ member(method(post), Request), !,
  reply_json_dict(_{error:true,message:"hello mister, you forgot to post your gameboard"}).

svg_handler(Request) :-
  member(method(post), Request), !,
  http_read_data(Request, Data, []),
  open_string(Data, Stream),
  parse_server(Stream, GameBoard),
  bestmove(GameBoard,3,Move),
  update_board(GameBoard,Move,NewBoard),
  write_svg(NewBoard),
  format('Content-type: image/svg+xml~n~n', []).
svg_handler(Request) :-
  \+ member(method(post), Request), !,
  reply_json_dict(_{error:true,message:"hello mister, you forgot to post your gameboard"}).


:- initialization http_daemon.

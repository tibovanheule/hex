#!/usr/bin/env swipl

/** <module> Server

@author Tibo Vanheule

*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_json)).

:- multifile http:location/3.
http:location(files, root(files), []).
user:file_search_path(folders, library('/website')).

:- http_handler(root(.), http_reply_from_files('./website', []), [prefix]).
:- http_handler(files(.), serve_files_in_directory(folders), [prefix]).
:- http_handler('/test', test_handler, []).

test_handler(Request) :-
  member(method(post), Request), !,
  http_read_json_dict(Request, _{userName:NameIn}),
  string_concat("Hello, ", NameIn, NameOut),
  reply_json_dict(_{computedString:NameOut}).
test_handler(Request) :-
  member(method(post), Request), !,
  http_read_json_dict(Request, _{userName:NameIn}),
  string_concat("Hello, ", NameIn, NameOut),
  reply_json_dict(_{computedString:NameOut}).

:- initialization http_daemon.

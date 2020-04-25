#!/usr/bin/env swipl

:- use_module(library(http/http_server)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

:- initialization http_server([port(3022)]).

:- http_handler(root(.), http_redirect(moved, location_by_id(home_page)), []).
:- http_handler(root(home), home_page, []).

home_page(_Request) :- reply_json(_{test:"hello world"}).

#!/usr/bin/env swipl

/** <module> Doc server
Deze module start een documentatie server op, deze documentatie wordt opgebouwd aan de hand van structured comments..
@author Tibo Vanheule
*/
:- use_module(library(pldoc/doc_library)).

:- doc_server(3021,[allow(localhost),edit(false)]).
:- consult('server.sh'), consult('main.pl').
:- doc_load_library.

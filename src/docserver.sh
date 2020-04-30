#!/usr/bin/env swipl

/** <module> Doc server
Deze module start een documentatie server op, deze documentatie wordt opgebouwd aan de hand van structured comments..
@author Tibo Vanheule
*/

:- doc_server(3021,[allow(localhost),edit(false)]).
:- use_module(library(pldoc/doc_library)).
:- consult('colors.pl'),consult('main.pl').
:- doc_load_library.

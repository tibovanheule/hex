#!/usr/bin/env swipl

/** <module> Doc server
Deze module start een documentatie server op, deze documentatie wordt opgebouwd aan de hand van structured comments..
@author Tibo Vanheule
*/
:- doc_server(3021,[edit(false)]).
:- consult('main.pl').
:- thread_get_message(Message).

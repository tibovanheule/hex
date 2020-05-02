:- module(test,[test/1]).
/** <module> Test
This module wil test a given board en give all positions.
NOT PLunit!

@author Tibo Vanheule

*/
:- use_module(write).
:- use_module(game).

test(Board) :- get_tiles(T),get_size(S),free(T,S,L),test_iter(Board,L).


test_iter(Board,[L]) :- !,test_write_board(Board,L),halt(0).
test_iter(Board,[H|L]) :- test_write_board(Board,H),nl, nl,write("~"),nl,nl,test_iter(Board,L).

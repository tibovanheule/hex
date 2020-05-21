:- module(test,[test/1,test_svg/1]).
/** <module> Test
This module wil test a given board en give all positions.
NOT PLunit!

@author Tibo Vanheule

*/
:- use_module(write).
:- use_module(game).

/**
 * test(-Arg:game)
 *
 * print all possible moves in text format, entry point for when running program with TEST-argument
 */
test(Board) :- get_tiles(T),get_size(S),free(T,S,L),nl,test_iter(Board,L),halt(0).

/**
 * testiter(-Arg:game,-Arg:List)
 *
 * Iterates over a list of moves, print the board in text form with appropieate seperator
 */
test_iter(Board,[L]) :- update_board(Board,L,NewBoard),write_board(NewBoard).
test_iter(Board,[H|L]) :- update_board(Board,H,NewBoard),write_board(NewBoard),write("~"),nl,test_iter(Board,L).


/**
 * test_svg(-Arg:game)
 *
 * print all possible moves as svg, entry point for when running program with TEST,SVG-arguments
 */
test_svg(Board) :- get_tiles(T),get_size(S),free(T,S,L),write('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">'),test_svg_iter(Board,0,L),nl,write('</svg>'),halt(0).


/**
 * testiter(-Arg:game,-Arg:Int,-Arg:List)
 *
 * Iterates over a list of moves, print the board as svg with appropieate seperator
 */
test_svg_iter(Board,Count,[L]) :-  Trans is Count * 300 + 1, update_board(Board,L,NewBoard),write_svg(NewBoard,Trans).
test_svg_iter(Board,Count,[H|L]) :- NewCount is Count + 1, Trans is Count * 300 + 1, update_board(Board,H,NewBoard),nl,write_svg(NewBoard,Trans),nl,test_svg_iter(Board,NewCount,L).

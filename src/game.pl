:- module(game,[get_tiles/1,get_size/1,get_number_of_tiles/1,map_to_letter/2,free/3,get_total_number_of_tiles/1,is_won/2]).
/** <module> Main
Deze module bevat het game board datastructuur en bijhorende operaties erop.

@author Tibo Vanheule

*/

/**
 * get_tiles(-Arg:game,-Arg:List)
 *
 * getter to get tiles of a board, returns only used tiles!
 */
get_tiles(Tiles) :- game(_,_,_,tiles(Tiles),_,_).

/**
 * get_size(-Arg:game,-Arg:size)
 *
 * getter to get size of a board.
 */
get_size(Size) :- game(Size,_,_,_,_,_).

/**
 * get_number_of_tiles(-Arg:game,-Arg:Int)
 *
 * getter to the number of used tiles of a board.
 */
get_number_of_tiles(Num) :- game(_,_,number_of_tiles(Num),_,_,_).

/**
 * get_total_number_of_tiles(-Arg:game,-Arg:Int)
 *
 * getter to the number of tiles of a board.
 */
get_total_number_of_tiles(Num) :- get_size(size(X,Y)), Num is X * Y.

/**
 * is_won(-Arg:game,-Arg:String)
 *
 * Check wether a player has won the game
 */
is_won(_,Out) :- Out = "undecided".

map_to_letter(Xi,X) :- char_code('A',Base), N is Base + Xi, char_code(X,N).

free(B,size(X2,Y2),L) :- findall(X/Y, (between(1, X2, X), between(1, Y2, Y),\+ member(tile(coordinate(X/Y),_),B)), L).

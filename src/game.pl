:- module(game,[get_tiles/1,get_size/1,get_number_of_tiles/1,map_to_letter/2,free/3,get_total_number_of_tiles/1,is_won/2,update_board/3]).
/** <module> Main
Deze module bevat het game board datastructuur en bijhorende operaties erop.

@author Tibo Vanheule

*/

:- use_module(library(lists)).

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
is_won(_,Out) :- Out =.. [state,undecided].

/**
 * map_to_letter(-Arg:Int,-Arg:letter)
 *
 * convert a number (representing a collum) to a letter
 */
map_to_letter(Xi,X) :- N is 65 + Xi, char_code(X,N).

/**
 * free(-Arg:game,-Arg:size,-Arg:List)
 *
 * Get all possible moves, sorted
 */
free(B,size(X2,Y2),L) :-  X2m1 is X2 -1, Y2m1 is Y2 - 1, setof(X/Y, (between(0, X2m1, X), between(0, Y2m1, Y),\+ member(tile(coordinate(X/Y),_),B)), L).
free(_,_,L) :- L = [].


update_board(game(S,turn(Turn),number_of_tiles(N),tiles(Tiles),_,Or),Move,NewBoard) :- Num is N + 1,append([tile(coordinate(Move),player(Turn))],Tiles,NewTiles),is_won(NewTiles,NewState), toggle_turn(Turn,NewTurn,Or), NewBoard =.. [game,S,NewTurn,number_of_tiles(Num),tiles(NewTiles),NewState,Or].

toggle_turn(Turn,NewTurn,orientation(X,Y)) :- Turn == X, NewTurn =.. [turn,Y].
toggle_turn(Turn,NewTurn,orientation(X,Y)) :- Turn == Y, NewTurn =.. [turn,X].

score([],0).

take_move(0/0,0).

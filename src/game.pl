:- module(game,[get_tiles/1,get_size/1,get_number_of_tiles/1,map_to_letter/2,free/3,get_total_number_of_tiles/1,is_won/4,update_board/3,update_board_dijkstra/3,get_turn/1,get_orientation/1]).
/** <module> Main
Deze module bevat het game board datastructuur en bijhorende operaties erop.

@author Tibo Vanheule

*/

:- use_module(library(lists)).
:- use_module(dijkstra).

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


get_turn(Turn) :- game(_,Turn,_,_,_,_).

get_orientation(OR) :- game(_,_,_,_,_,OR).

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
 * is_won(-Arg:Tiles,-Arg:Orientation,-Arg:String)
 *
 * Check wether a player has won the game
 */
is_won(Tiles,size(W,_),orientation(P1,_),Out) :- Sp1 is W + 1,get_dist(Sp1/Sp1,0/(-1),Tiles,Dist,P1,true,Sp1),Dist @=< 0, string_concat("won by ",P1,State),Out =.. [state,State].
is_won(Tiles,size(_,H),orientation(_,P2),Out) :- Sp1 is H + 1,get_dist(Sp1/Sp1,(-1)/0,Tiles,Dist,P2,true,Sp1),Dist @=< 0, string_concat("won by ",P2,State),Out =.. [state,State].
is_won(_,_,_,Out) :- Out =.. [state,undecided].

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
free(B,size(X2,Y2),L) :-  X2m1 is X2 -1, bagof(X/Y, (between(0, X2m1, X), between(1, Y2, Y),\+ member(tile(coordinate(X/Y),_),B)), L).
free(_,_,L) :- L = [].


update_board(game(S,turn(Turn),number_of_tiles(N),tiles(Tiles),_,Or),Move,NewBoard) :- Num is N + 1,append([tile(coordinate(Move),player(Turn))],Tiles,NewTiles),is_won(NewTiles,S,Or,NewState), toggle_turn(Turn,NewTurn,Or), NewBoard =.. [game,S,NewTurn,number_of_tiles(Num),tiles(NewTiles),NewState,Or].

update_board_dijkstra(game(S,turn(Turn),number_of_tiles(N),tiles(Tiles),State,Or),Move,NewBoard) :- length(Move,Len),Num is N + Len,convlist([X/Y,B]>>(Y2 is Y + 1,B =.. [tile,coordinate(X/Y2),player(green)]),Move, L),append(L,Tiles,NewTiles), toggle_turn(Turn,NewTurn,Or), NewBoard =.. [game,S,NewTurn,number_of_tiles(Num),tiles(NewTiles),State,Or].




toggle_turn(Turn,NewTurn,orientation(X,Y)) :- Turn == X, NewTurn =.. [turn,Y].
toggle_turn(Turn,NewTurn,orientation(X,Y)) :- Turn == Y, NewTurn =.. [turn,X].

score([],0).

take_move(0/0,0).

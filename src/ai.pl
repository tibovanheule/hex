:- module(ai,[bestmove/2])
/** <module> AI
Zoekt de beste mogelijke zet voor een gegeven bord

@author Tibo Vanheule

*/
:- use_module(game).


/**
 * calculate_score(-Arg:board,-Arg:int).
 *
 * berkent een score voor een nieuw bord.
 */
calculate_score(_,A) :- A = 0.

/**
 * cache_score(-Arg:board,-Arg:int).
 *
 * Kijk in de prolog database als een bord al een vroeger berkent score heeft, anders bereken die.
 */
cache_score(game(_,_,_,Tiles,_,-),S) :- sort(Tiles,Id),score(ID,S).
cache_score(Board,S) :- calculate_score(Board,S),add_to_cache(Board,S).

/**
 * bestmove(-Arg:board,-Arg:int).
 *
 * Zoekt de best mogelijke move,
 * @Arg Board spelbord
 * @Arg Depth maximum diepte van de spelboom
 */
add_to_cache(game(_,_,_,Tiles,_,_),S) :- sort(Tiles,Id), X =.. [score,Id,S],asserta(X).


/**
 * board_is_full(-Arg:board).
 *
 * bekijkt als er nog mogelijke zetten zijn of het bord vol is.
 */
board_is_full(game(S,_,_,Tiles,_,_)) :- free(S,Tiles,L), length(L,0).

/**
 * bestmove(-Arg:board,-Arg:int).
 *
 * Zoekt de best mogelijke move,
 * @Arg Board spelbord
 * @Arg Depth maximum diepte van de spelboom
 */
bestmove(Board,Depth,Move) :- boom(Board,Depth,Alpha,Beta,true,Move).

/**
 * boom(-Arg:board,-Arg:int).
 *
 */
boom(Board,_,_,_,S,_,Best) :- board_is_full(Board),!,score(S). % geen zetten meer mogelijk
boom(Board,0,_,_,S,_,Best) :- !, score(Board,S). % gewenste diepte bereikt, bereken score
boom(game(S,T,N,Tiles,State,Or),Depth,Alpha,Beta,Best,true,Best) :- free(S,Tiles,L), max_recursion(Board,Depth,Beta,Alpha,L,Best).
boom(game(S,T,N,Tiles,State,Or),Depth,Alpha,Beta,Best,false,Best) :- free(S,Tiles,L), min_recursion(Board,Depth,Beta,Alpha,L,Best).

max_recursion(_,_,Beta,Alpha,_,Best) :- Beta <= Alpha,!.
max_recursion(_,_,Beta,Alpha,[],Best) :- !.
max_recursion(Board,Depth,Beta,Alpha,[H|T],Best) :-  update_board(Board,H), Dnew is Depth - 1, boom(Board,Dnew,Alpha,Beta,false), max_recursion(Board,Depth,Beta,Alpha,T,max())
                    bestValue = max(bestValue, alphaBetaPrunedMiniMax(board: updatedBoard, maximizingPlayer: false, depth: depth-1, alpha: alpha, beta: beta))
                    alpha = max(alpha, bestValue)

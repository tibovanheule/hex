:- module(ai,[bestmove/3]).
/** <module> AI
Zoekt de beste mogelijke zet voor een gegeven bord

@author Tibo Vanheule

*/
:- use_module(game).
:- use_module(dijkstra).
:- use_module(library(lists)).


/**
 * calculate_score(-Arg:board,-Arg:int).
 *
 * berkent een score voor een nieuw bord.
 */
calculate_score(Tiles,Score) :- get_size(size(W,H)),get_turn(turn(T)),get_orientation(orientation(P1,_)), P1 == T, Sp1 is H + 1, numlist(0,W,L1), get_player_score(L1,P1,Sp1,0/(-1),H,Tiles, Score).
calculate_score(Tiles,Score) :- get_size(size(W,H)),get_turn(turn(T)),get_orientation(orientation(_,P2)), P2 == T, Sp2 is W+1, numlist(0,H,L2), get_player_score(L2,P2,Sp2,(-1)/0,W,Tiles, Score).

get_player_score(L,P,Sp1,To,Dir,Tiles,Score) :- convlist([X,Dist]>>(get_dist(X/Sp1,To,Tiles,Dist,P,true,Dir)),L,O), min_list(O,Score).

:- dynamic score/2.
score([],0).

/**
 * cache_score(-Arg:board,-Arg:int).
 *
 * Kijk in de prolog database als een bord al een vroeger berkent score heeft, anders bereken die.
 */
cache_score(game(_,_,_,tiles(Tiles),_,_),S) :- sort(Tiles,Id),score(Id,S).
cache_score(game(_,_,_,tiles(Tiles),_,_),S) :- calculate_score(Tiles,S),add_to_cache(Tiles,S).

/**
 * add_to_cache(-Arg:Tiles,-Arg:Int).
 *
 * Zoekt de best mogelijke move,
 * @Arg Board spelbord
 * @Arg Depth maximum diepte van de spelboom
 */
add_to_cache(Tiles,S) :- sort(Tiles,Id), X =.. [score,Id,S],asserta(X).


/**
 * bestmove(-Arg:board,-Arg:int).
 *
 * Zoekt de best mogelijke move,
 * @Arg Board spelbord
 * @Arg Depth maximum diepte van de spelboom
 */
bestmove(Board,Depth,Move) :- get_tiles(T),get_size(S),free(T,S,L),best(L, Move,Board,true,_, Depth).

/**
 * boom(-Arg:board,-Arg:int).
 *
 */
boom(_,_,Depth,_,Board,Val):- Depth @=< 0,!, cache_score(Board,Val).
boom(Pos,Maximizing,Depth,Move,Board,Val) :- update_board(Board,Pos,NewBoard),moves(NewBoard,Moves),best(Moves,Move,NewBoard,Maximizing,Val,Depth).
boom(_,_,_,_,Board,Val):- cache_score(Board,Val).

moves(game(Size,_,_,Tiles,_,_),Moves) :- free(Tiles,Size,L),length(L,Len),Len>0,Moves = L.

best([Pos], Pos,Board,Maximizing,Val, Depth) :-
    NDepth is Depth -1,
    boom(Pos,\+ Maximizing,NDepth,_,Board,Val), !.

best([Pos1 | PosList], BestPos,Board,Maximizing, BestVal,Depth) :-
    NDepth is Depth - 1,
    boom(Pos1,\+ Maximizing,NDepth,_,Board,Val1),
    best(PosList, Pos2,Board, \+ Maximizing, Val2,NDepth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal, Maximizing).


betterOf(Pos0, Val0, _, Val1, Pos0, Val0,false) :- Val0 > Val1, !.
betterOf(Pos0, Val0, _, Val1, Pos0, Val0,true) :- Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1,_).

:- module(ai,[bestmove/3]).
/** <module> AI
Zoekt de beste mogelijke zet voor een gegeven bord

@author Tibo Vanheule

*/
:- use_module(game).
:- use_module(dijkstra).
:- use_module(library(lists)).
/**
 * bestmove(-Arg:board,-Arg:int,-Arg:int).
 *
 * Zoekt de best mogelijke move,roept de spelboom-structuur op
 */
bestmove(Board,Depth,Move) :- get_tiles(T),get_size(S),free(T,S,L),best(L, Move,Board,true,_, Depth).

/**
 * calculate_score(-Arg:board,-Arg:int).
 *
 * berkent een score voor een nieuw bord, waarvan de state al dan niet decided is.
 */
calculate_score(Tiles,Score) :- get_size(size(W,H)),get_turn(turn(T)),get_orientation(orientation(P1,P2)), P1 == T, calculate_top_down(W,Tiles,P1,Player), calculate_left_right(H,Tiles,P2,Enemy), Score is Player - Enemy.
calculate_score(Tiles,Score) :- get_size(size(W,H)),get_turn(turn(T)),get_orientation(orientation(P1,P2)), P2 == T,calculate_top_down(W,Tiles,P1,Enemy), calculate_left_right(H,Tiles,P2,Player), Score is Player - Enemy.

/**
 * calculate_top_down(-Arg:int,-Arg:tiles,-Arg:color,-Arg:int).
 *
 * bereken de distance van boven naar beneden, dit loopt gelijk met het aantal vakjes dat niet gekleurd zijn op het korste weg
 */
calculate_top_down(W,Tiles,P1,Score) :- Sp1 is W + 1, get_dist(Sp1/Sp1,0/(-1),Tiles,Score,P1,true,Sp1).

/**
 * calculate_left_right(-Arg:int,-Arg:tiles,-Arg:color,-Arg:int).
 *
 * bereken de distance van links naar rechts, dit loopt gelijk met het aantal vakjes dat niet gekleurd zijn op het korste weg
 */
calculate_left_right(H,Tiles,P2,Score) :-  Sp1 is H + 1, get_dist(Sp1/Sp1,(-1)/0,Tiles,Score,P2,false,Sp1).

/**
 * score(-Arg:tiles,-Arg:int).
 *
 * Predicaat gebruikt als cache, vermijden van dijkstra uit te voeren als dit al eens uitgerenkend werd.
 */
:- dynamic score/2.
score([],0).

/**
 * cache_score(-Arg:board,-Arg:int).
 *
 * Kijk in de prolog database als een bord al een vroeger berkend score heeft, anders bereken die.
 */
cache_score(game(_,_,_,tiles(Tiles),_,_),S) :- sort(Tiles,Id),score(Id,S).
cache_score(game(_,_,_,tiles(Tiles),_,_),S) :- calculate_score(Tiles,S),add_to_cache(Tiles,S).

/**
 * add_to_cache(-Arg:Tiles,-Arg:Int).
 *
 * Voeg een score toe aan de prolog database.
 */
add_to_cache(Tiles,S) :- sort(Tiles,Id), X =.. [score,Id,S],asserta(X).

/**
 * boom(-Arg:board,-Arg:int).
 *
 * Pas een bord aan, berekend alle mogelijke zetten en roept best aan.
 * Tenzij de diepte al bereikt is of geen zetten meer mogelijk zijn. Dan wordt dijkstra gebruikt.
 */
boom(_,_,Depth,_,Board,Val):- Depth @=< 0,!, cache_score(Board,Val).
boom(Pos,Maximizing,Depth,Move,Board,Val) :- update_board(Board,Pos,NewBoard),moves(NewBoard,Moves),best(Moves,Move,NewBoard,Maximizing,Val,Depth).
boom(_,_,_,_,Board,Val):- cache_score(Board,Val).

/**
 * moves(-Arg:board,-Arg:List).
 *
 * Geeft voor een gegeven bord het aantal mogelijke zetten, faild wanneer er geen zijn.
 */
moves(game(Size,_,_,Tiles,_,_),Moves) :- free(Tiles,Size,L),length(L,Len),Len>0,Moves = L.

/**
 * best(-Arg:List,-Arg:int/int,-Arg:board,-Arg:boolean,-Arg:int,-Arg:int).
 *
 * recursie over een lijst moves, zoek de pos met de beste value uit een lijst.
 */
best([Pos], Pos,Board,Maximizing,Val, Depth) :-
    NDepth is Depth -1,
    boom(Pos,\+ Maximizing,NDepth,_,Board,Val), !.

best([Pos1 | PosList], BestPos,Board,Maximizing, BestVal,Depth) :-
    NDepth is Depth - 1,
    boom(Pos1,\+ Maximizing,NDepth,_,Board,Val1),
    best(PosList, Pos2,Board, \+ Maximizing, Val2,NDepth),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal, Maximizing).

    /**
     * betterOf(-Arg:int/int,-Arg:int,-Arg:int/int,-Arg:int,-Arg:int,-Arg:int,-Arg:boolean).
     *
     * recursie over een lijst moves, zoek de pos met de beste value uit een lijst.
     */
betterOf(Pos0, Val0, _, Val1, Pos0, Val0,false) :- Val0 > Val1, !.
betterOf(Pos0, Val0, _, Val1, Pos0, Val0,true) :- Val0 < Val1, !.
betterOf(_, _, Pos1, Val1, Pos1, Val1,_).

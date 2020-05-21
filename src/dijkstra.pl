:- module(dijkstra,[get_path/7,get_dist/7]).
/** <module> Dijsktra
Bepaalt de korste mogelijk pad in een bord via het dijkstra algoritme.

@author Tibo Vanheule

*/
:- use_module(game).

/**
 * get_dist(-Arg:int/int,-Arg:list,-Arg:color,-Arg:boolean,-Arg:int)
 *
 * Zoek het korste pad en geef een lijst van coordinaten (pad) terug.
 */
get_path(From, To,Tiles,Path,Player,X,Size) :-
	traverse(From,Tiles,Player,X,Size),
	edge([To|ReversedPath], _),
    reverse([To|ReversedPath], Path).

get_path(_,_,_,Path,_,_) :- Path = [].

/**
 * get_dist(-Arg:int/int,-Arg:list,-Arg:color,-Arg:boolean,-Arg:int)
 *
 * Zoek het korste pad en geef de distance terug.
 */
get_dist(From, To,Tiles,Dist,Player,X,Size) :-
	traverse(From,Tiles,Player,X,Size),
	edge([To|_], Dist).

    /**
     * traverse(-Arg:int/int,-Arg:list,-Arg:color,-Arg:boolean,-Arg:int)
     *
     * Dijkstra lgoritme, bfs alle buren bezoeken.
     */
traverse(From,Tiles,Player,X,Size) :-
	retractall(edge(_,_)),
	traverse(From,[],0,Tiles,Player,X,Size).
traverse(_,_,_,_,_):-!.

traverse(From, Path, Dist,Tiles,Player,X,Size) :-
	neighbour(From,T,D,Tiles,Player,X,Size),
	\+ member(T, Path),
    NDist is Dist + D,
	shorter_path([T,From|Path], NDist),
	traverse(T,[From|Path],NDist,Tiles,Player,X,Size).

    /**
     * shorter_path(-Arg:list,-Arg:int)
     * bewaard path en distance van dijkstra.
     * Als er een entry in de prolog DB bestaat, update die anders creeër die..
     */
shorter_path([H|Path], Dist) :-
	edge([H|_], D), !, Dist < D,
	retract(edge([H|_],_)),
	assert(edge([H|Path], Dist)).

shorter_path(Path, Dist) :-	assert(edge(Path,Dist)).

/**
 * check_cell(-Arg:int/int,-Arg:color,-Arg:list)
 *
 * Bepaalt Als een coordinaat in het veld ligt en bezocht mag worden.
 */
check_cell(X2/Y2,Tiles,P,S) :- check_cell1(X2/S), check_cell1(Y2/S),tile_free(X2/Y2,P,Tiles).

/**
 * check_cell1(-Arg:int/int)
 *
 * Bepaalt Als een coordinaat in het veld ligt.
 */
check_cell1(X/S) :-     0 =< X, X =< S.

/**
 * tile_free(-Arg:int/int,-Arg:color,-Arg:list)
 *
 * Bepaalt Als een coordinaat bezocht mag worden.
 */
tile_free(X2/Y2,P,Tiles) :- member(tile(coordinate(X2/Y2),player(P2)),Tiles), compare(=, P2, P).
tile_free(X2/Y2,_,Tiles) :- \+ member(tile(coordinate(X2/Y2),_),Tiles).

/**
 * neighbour(-Arg:int/int,-Arg:int/int,-Arg:int,-Arg:list,-Arg:color,-Arg:boolean,-Arg:int)
 *
 * Bepaalt voor elk coordinaat alle mogelijke buren die valid zijn in het bord
 */
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- Y2 is Y1+1,X2 is X1, check_cell(X2/Y2,Tiles,Player,S), distance(X2/Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- Y2 is Y1-1,X2 is X1, check_cell(X2/Y2,Tiles,Player,S), distance(X2/Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1+1, Y2 is Y1, check_cell(X2/Y2,Tiles,Player,S), distance(X2/Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1-1, Y2 is Y1, check_cell(X2/Y2,Tiles,Player,S), distance(X2/Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1+1, Y2 is Y1-1, check_cell(X2/Y2,Tiles,Player,S), distance(X2/Y2,D,Player,Tiles,S,X).
%% Base player 1 boven
neighbour(_/Y1,X2/Y3, D,_,_,X,_) :- X, Y2 is Y1-1,Y2 < 1, Y3 = -1,X2 = 0, D= 0.
%% Base player 2 links
neighbour(X1/_,X3/Y2, D,_,_,X,_) :- not(X), X2 is X1-1,X2 < 1, X3 = -1, Y2 = 0,D = 0.

/**
 * distance(-Arg:int/int,-Arg:int,-Arg:color,-Arg:list,-Arg:int,-Arg:boolean)
 *
 * Bepaalt het gewicht naar een gegeven coordinaat.
 */
distance(X2/Y2,D,P,Tiles,_,_) :- member(tile(coordinate(X2/Y2),player(P)),Tiles), D = 0.
distance(_/Y2,D,_,_,S,X) :- X, Y2 @>= S, D = 0.
distance(X2/_,D,_,_,S,X) :- \+ X, X2 @>= S, D = 0.
distance(_/_,D,_,_,_,_) :- D = 1.

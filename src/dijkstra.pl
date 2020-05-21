:- module(dijkstra,[get_path/7,get_dist/7]).
/** <module> Dijsktra
Bepaalt de korste mogelijk pad in een bord.

@author Tibo Vanheule

*/
:- use_module(game).

get_path(From, To,Tiles,Path,Player,X,Size) :-
	traverse(From,Tiles,Player,X,Size),
	edge([To|ReversedPath], _),
    reverse([To|ReversedPath], Path).

get_path(_,_,_,Path,_,_) :- Path = [].

get_dist(From, To,Tiles,Dist,Player,X,Size) :-
	traverse(From,Tiles,Player,X,Size),
	edge([To|_], Dist).


traverse(From,Tiles,Player,X,Size) :-
	retractall(edge(_,_)),
	traverse(From,[],0,Tiles,Player,X,Size).
traverse(_,_,_,_,_):-!.

traverse(From, Path, Dist,Tiles,Player,X,Size) :-
	neighbour(From,T,D,Tiles,Player,X,Size),
	\+ member(T, Path),
    NDist is Dist + D,
	shorterPath([T,From|Path], NDist),
	traverse(T,[From|Path],NDist,Tiles,Player,X,Size).

shorterPath([H|Path], Dist) :-
	edge([H|_], D), !, Dist < D,
	retract(edge([H|_],_)),
	assert(edge([H|Path], Dist)).

shorterPath(Path, Dist) :-	assert(edge(Path,Dist)).


checkCell(X2/Y2,Tiles,P,S) :- checkCell1(X2,S), checkCell1(Y2,S),tile_free(X2,Y2,P,Tiles).
checkCell1(X,S) :-     0 =< X, X =< S.

tile_free(X2,Y2,P,Tiles) :- member(tile(coordinate(X2/Y2),player(P2)),Tiles), compare(=, P2, P).
tile_free(X2,Y2,_,Tiles) :- \+ member(tile(coordinate(X2/Y2),_),Tiles).

neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- Y2 is Y1+1,X2 is X1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- Y2 is Y1-1,X2 is X1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1+1, Y2 is Y1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1-1, Y2 is Y1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles,S,X).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,X,S) :- X2 is X1+1, Y2 is Y1-1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles,S,X).
%% Base player 1 onder
neighbour(_/Y1,X2/Y3, D,_,_,X,_) :- X, Y2 is Y1-1,Y2 < 1, Y3 = -1,X2 = 0, D= 0.
%% Base player 2 links
neighbour(X1/_,X3/Y2, D,_,_,X,_) :- \+ (X), X2 is X1-1,X2 < 1, X3 = -1, Y2 = 0,D = 0.

distance(X2,Y2,D,P,Tiles,_,_) :- member(tile(coordinate(X2/Y2),player(P)),Tiles), D = 0.
distance(_,Y2,D,_,_,S,X) :- X, Y2 @>= S, D = 0.
distance(X2,_,D,_,_,S,X) :- \+ X, X2 @>= S, D = 0.
distance(_,_,D,_,_,_,_) :- D = 1.

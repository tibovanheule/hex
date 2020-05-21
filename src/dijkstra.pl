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


shorterPath([H|Path], Dist) :-		       % if there is a fact edge(H|_, D), this means that we know the distance from node From to node H.
	edge([H|_], D), !, Dist < D,          %  But if the distance D is greater than the new distance Dist that we discovered,
	retract(edge([H|_],_)),				%  then we are going to remove (assert) the fact edge([H|_], D) and add a new one edge([H|Path], Dist).
	assert(edge([H|Path], Dist)).		% This is the shortest path that we have found so far to reach the node H.

shorterPath(Path, Dist) :-		       % Let's create a new fact to memorize the path from the origin node From, to another node.
	assert(edge(Path,Dist)).			% Keep track the distance Dist of that path.


checkCell(X2/Y2,Tiles,P,S) :- checkCell1(X2,S), checkCell1(Y2,S),tile_free(X2,Y2,P,Tiles).
checkCell1(X,S) :-     0 =< X, X =< S.

tile_free(X2,Y2,P,Tiles) :- member(tile(coordinate(X2/Y2),player(P2)),Tiles), compare(=, P2, P).
tile_free(X2,Y2,_,Tiles) :- \+ member(tile(coordinate(X2/Y2),_),Tiles).


%% Base player 1 boven naar onder
neighbour(_/Y1,X2/Y3, D,_,_,X,_) :- X, Y2 is Y1-1,Y2 < 1, Y3 = -1,X2 = 0, D= 0.
%% Base player 2 links en rechts
neighbour(X1/_,X3/Y2, D,_,_,X,_) :- \+ (X), X2 is X1-1,X2 < 1, X3 = -1, Y2 = 0,D = 0.
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,_,S) :- Y2 is Y1+1,X2 is X1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,_,S) :- Y2 is Y1-1,X2 is X1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,_,S) :- X2 is X1+1, Y2 is Y1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,_,S) :- X2 is X1-1, Y2 is Y1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles).
neighbour(X1/Y1,X2/Y2, D,Tiles,Player,_,S) :- X2 is X1+1, Y2 is Y1-1, checkCell(X2/Y2,Tiles,Player,S), distance(X2,Y2,D,Player,Tiles).

distance(X2,Y2,D,P,Tiles) :- member(tile(coordinate(X2/Y2),player(P)),Tiles), D = 0.
distance(_,_,D,_,_) :- D = 1.

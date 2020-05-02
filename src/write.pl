:- module(write,[test_write_board/2,write_error/1]).
/** <module> Write
This module wil print a board, test, svg ...

@author Tibo Vanheule

*/

test_write_board(game(size(X,Y),turn(Turn),number_of_tiles(N),tiles(Tiles),state(State),orientation(P1,P2)),T) :-
    Num is N + 1,
    append(Tiles,[tile(coordinate(T),player(Turn))],NewList),
    format(atom(Out), 'state: ~s~n\c
    turn: ~s~n\c
    size: ~d * ~d~n\c
    orientation: ~s * ~s~n\c
    tiles: ~d~n', [State,Turn,X,Y,P1,P2,Num]),
    write(Out),
    write_tile(NewList).

write_tile([tile(coordinate(Xi/Y),player(P))]) :- map_to_letter(Xi,X),format(atom(Out), '\t(~s~d) -> ~s~n', [X,Y,P]),write(Out).
write_tile([tile(coordinate(Xi/Y),player(P))|L]) :- map_to_letter(Xi,X),format(atom(Out), '\t(~s~d) -> ~s~n', [X,Y,P]),write(Out),write_tile(L).

write_error(X) :- set_output(user_error),write(X),halt(4).

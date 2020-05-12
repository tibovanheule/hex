:- module(write,[test_write_board/1,write_error/1,write_svg/2]).

:- use_module(library(http/http_json)).
/** <module> Write
This module wil print a board, test, svg ...

@author Tibo Vanheule

*/

test_write_board(game(size(X,Y),turn(Turn),number_of_tiles(N),tiles(Tiles),state(State),orientation(P1,P2))) :-
    format(atom(Out), 'state: ~s~n\c
    size: ~d * ~d~n\c
    orientation: ~s * ~s~n\c
    turn: ~s~n', [State,X,Y,P1,P2,Turn]),
    format(atom(TileOut), 'tiles: ~d~n', [N]),
    write(TileOut),
    write_tile(Tiles),
    write(Out).

write_tile([tile(coordinate(Xi/Y),player(P))]) :- map_to_letter(Xi,X),format(atom(Out), '\t(~s~d) -> ~s~n', [X,Y,P]),write(Out).
write_tile([tile(coordinate(Xi/Y),player(P))|L]) :- map_to_letter(Xi,X),format(atom(Out), '\t(~s~d) -> ~s~n', [X,Y,P]),write(Out),write_tile(L).

write_error(X) :- set_output(user_error),write(X),halt(4).


write_svg(Board,Out) :- format(atom(Out),'<svg width="500" height="300" viewbox="0 0 30.3108891323 17.5" xmlns="http://www.w3.org/2000/svg">

  <defs>
    <style>
        /* Default tile colours, to make them look like a grid*/
        use:not([fill]) {
            fill: #ECECEC;
        }

        text.row_or_col {
            font-size: 0.3px;
            font-weight: bold;
            font-family: sans;
            fill: white;
            stroke-width: 0.025px;
            paint-order: stroke;
        }
    </style>
    <!-- The tile that is reused (result of right multiplication with inverse of matrix transform)-->
    <polygon id="tile" points=" 0.35,0.35 0.68,-0.35 0.35,-0.68 -0.35,-0.35 -0.68,0.35 -0.35,0.68 " stroke-width="0.01" stroke="black"></polygon>
   </defs>

    <g transform="matrix(1.73205080756 0 0.86602540378 1.5 3.46410161512 2.5)">
    </g>
</svg>',[]).

json_write_board(game(size(X,Y),turn(Turn),number_of_tiles(N),tiles(Tiles),state(State),orientation(P1,P2)))

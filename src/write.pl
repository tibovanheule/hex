:- module(write,[write_board/1,write_error/1,write_svg/2,json_write_board/2]).

/** <module> Write
This module wil print a board, test, svg, json ...

@author Tibo Vanheule

*/
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(game).

/**
 * write_board(-Arg:board)
 *
 * print a game board als text
 */
write_board(game(size(X,Y),turn(Turn),number_of_tiles(N),tiles(Tiles),state(State),orientation(P1,P2))) :-
    format('tiles: ~d~n', [N]),
    maplist(write_tile,Tiles),
    format('state: ~s~n\c
    size: ~d * ~d~n\c
    orientation: ~s * ~s~n\c
    turn: ~s~n', [State,X,Y,P1,P2,Turn]).

    /**
     * write_tile(-Arg:list)
     *
     *  itereer over de lijst met tegels, print ze uit als text
     */
write_tile(tile(coordinate(Xi/Y),player(P))) :- map_to_letter(Xi,X),format('    (~s~d) -> ~s~n', [X,Y,P]).


/**
 * write_error(-Arg:String)
 *
 *schrijf een error uit en stop het programa met een foutcode
 */
write_error(X) :- set_output(user_error),write(X),halt(4).


/**
 * write_svg(-Arg:Board)
 *
 * Print board as svg.
 */
write_svg(game(size(X,Y),turn(_),number_of_tiles(_),tiles(Tiles),state(_),orientation(P1,P2)),Trans) :- Xm1 is X - 1, Ym1 is Y -1, format('<svg width="500" height="301.15830115830113" viewBox="0 0 32.375 19.5" y="~d" xmlns="http://www.w3.org/2000/svg">

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
    <!--Player names and sidebars-->
    <g>
        <text text-anchor="start" fill="~s" font-size="0.5" y="-1.1" x="-0.5">Player 1</text>
        <polygon fill="~s" points="-1,-1 0,0 ~d,0 ~d,-1"></polygon>
        <polygon fill="~s" points="-1,~d 0,~d ~d,~d ~d,~d"></polygon>
        <text text-anchor="start" fill="~s" font-size="0.5" transform="rotate(90)" y="-~d.1" x="-0.5">Player 2</text>
        <polygon fill="~s" points="-1,-1 0,0 0,~d -1,~d"></polygon>
        <polygon fill="~s" points="~d,-1 ~d,0 ~d,~d ~d,~d"></polygon>
    </g>
    <!--Row and col numberings-->
    ',[Trans,P1,P1,Xm1,X,P1,Y,Ym1,Xm1,Ym1,X,Y,P2,X,P2,Ym1,Y,P2,X,Xm1,Xm1,Ym1,X,Y]),
svg_row(Ym1,Rows),
svg_col(Xm1,Cols),
svg_tile_col(Xm1,Ym1,Tiles,T3),
atomics_to_string([Rows,Cols,T3,'</g>\n</svg>'], Out),
write(Out).

/**
 * svg_row(-Arg:int,-Arg:String)
 *
 * print row label
 */
svg_row(0,Out) :- Out = '\t<text class="row_or_col" x="-0.95" y="0">0</text>\n'.
svg_row(Y,Out) :- format(atom(Now),'\t<text class="row_or_col" x="-0.95" y="~d">~d</text>\n',[Y,Y]), Ym1 is Y -1, svg_row(Ym1,Rest), string_concat(Rest,Now,Out).

/**
 * svg_col(-Arg:int,-Arg:String)
 *
 * print collum label
 */
svg_col(0,Out) :- Out = '\t<text class="row_or_col" y="-0.65" x="0">A</text>\n'.
svg_col(Y,Out) :- Yascii is Y + 65, char_code(Yletter,Yascii), format(atom(Now),'\t<text class="row_or_col" y="-0.65" x="~d">~s</text>\n',[Y,Yletter]), Ym1 is Y -1, svg_col(Ym1,Rest), string_concat(Rest,Now,Out).

/**
 * svg_tile_col(-Arg:int,-Arg:int,-Arg:list,-Arg:String)
 *
 * itterate over a collumn
 */
svg_tile_col(0,Y,Tiles,Out) :- svg_tile_row(0,Y,Tiles,Out).
svg_tile_col(X,Y,Tiles,Out) :- Xm1 is X - 1,svg_tile_row(X,Y,Tiles,B), svg_tile_col(Xm1,Y,Tiles,T), string_concat(B,T,Out).

/**
 * svg_tile_row(-Arg:int,-Arg:int,-Arg:list,-Arg:String)
 *
 * prin a single tile of a collum as svg
 */
svg_tile_row(X,0,Tiles,Out) :- \+ member(tile(coordinate(X/1),_),Tiles), format(atom(Out),'\t<use href="#tile" x="~d" y="~d"></use>\n',[X,0]).
svg_tile_row(X,Y,Tiles,Out) :- Yp1 is Y +1,\+ member(tile(coordinate(X/Yp1),_),Tiles), Ym1 is Y - 1 ,format(atom(B),'\t<use href="#tile" x="~d" y="~d"></use>\n',[X,Y]), svg_tile_row(X,Ym1,Tiles,T), string_concat(B,T,Out).
svg_tile_row(X,0,Tiles,Out) :- member(tile(coordinate(X/1),player(P)),Tiles), format(atom(Out),'\t<use href="#tile" x="~d" y="~d" fill="~s"></use>\n',[X,0,P]).
svg_tile_row(X,Y,Tiles,Out) :- Yp1 is Y +1,member(tile(coordinate(X/Yp1),player(P)),Tiles), Ym1 is Y - 1 ,format(atom(B),'\t<use href="#tile" x="~d" y="~d" fill="~s"></use>\n',[X,Y,P]), svg_tile_row(X,Ym1,Tiles,T), string_concat(B,T,Out).

/**
 * json_write_board(-Arg:game,-Arg:Json)
 *
 * game to json
 */
json_write_board(game(size(X,Y),turn(Turn),number_of_tiles(N),tiles(Tiles),state(State),orientation(P1,P2)), Json) :- tiles_to_json(Tiles,Tjson), Json = _{width:X,height:Y,turn:Turn,number_of_tiles:N,moves:Tjson,state:State,player_x:P1,player_y:P2}.

/**
 * tiles_to_json(-Arg:List,-Arg:List)
 *
 *  iterate over a list of tiles to print them as json
 */
tiles_to_json([tile(coordinate(Xi/Y),player(P))],Json) :- map_to_letter(Xi,X), Json = [_{xi:Xi,x:X,y:Y,player:P}].
tiles_to_json([tile(coordinate(Xi/Y),player(P))|L],Json) :- map_to_letter(Xi,X), tiles_to_json(L,List), append([_{xi:Xi,x:X,y:Y,player:P}],List,Json).

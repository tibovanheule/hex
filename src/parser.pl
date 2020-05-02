:- module(parser, [parse/1,parseTest/2]).

/** <module> Parser
Behandelt de stream en maakt een Game-datastructur aan.

@author Tibo Vanheule

*/
:- use_module(library(dcg/basics)).
:- use_module(colors).

/**
 * parse(-Arg:Game)
 *
 * gets, the input stream and applies the grmmar, gives back an game.
 * If it fails then we write an error and quit the program
 */
parse(Retval) :- phrase_from_stream(gram(Retval),user_input).
parse(_) :-
    set_output(user_error),
    write("Failed to parse."),
    halt(4).

/**
 * parseTest(-Arg:Game)
 *
 * To simplify writing unit test of the parser, we define an access method to parse an stream /= user_stream
 */
parseTest(Retval,S) :- phrase(gram(Retval),S).

/**
 * gram(-Arg:game)
 *
 * gramatica entry point all valid parse files.
 */
gram(Game) --> size(Size), turn(Turn), tiles(Number_of_tiles,Tiles), state(State),orientation(Or),!, {samecolor(Tiles,Or), Game =.. [game,Size,Turn,Number_of_tiles,Tiles,State,Or] }.

/**
 * verify(-Arg:number_of_tiles,-Arg:tiles)
 *
 * verify that a color of a player is in the orientation list.
 */
verify(List,tile(_,player(X))) :- member(X,List).

/**
 * samecolor(-Arg:number_of_tiles,-Arg:tiles)
 *
 * for every tile in the tiles list check if it is a member of the orientation.
 */
samecolor(tiles(Tiles),orientation(X,Y)) :- maplist(verify([X,Y]), Tiles).

/**
 * tiles(-Arg:number_of_tiles,-Arg:tiles)
 *
 * Get the number of tiles, and parse every following tile.
 */
tiles(Size,T) -->
     "tiles:",
     b,
     integer(S),
     tile(S,Tiles),
     {Size =.. [number_of_tiles,S], T =.. [tiles,Tiles] }.

/**
 * tile(-Arg:Int,-Arg:List)
 *
 * Predicate orientation, gramtica regel voor de tegels de parsen
 */
tile(0,_) --> [].
tile(N,Tiles) -->
    b,
    "(",
    nonblank(X), integer(Y),
    ")",
    b,
    "->",
    b,
    nonblanks(P),
    b,
    { N2 is N - 1, atom_codes(Player,P), color(Player),!},
    tile(N2,L),
    {
      atom_codes(Kol,[X]),
      char_code('A',Base),
      atom_codes(Kol,Val),
      Koli is Val - Base + 1,
      append([tile(coordinate(Koli/Y),player(Player))],L,Tiles)
     }.

/**
 * turn(-Arg:turn)
 *
 * Predicate turn, gramtica regel voor de beurt van een speler te parsen.
 */
turn(Turn) --> "turn:",b,nonblanks(P),b, { atom_codes(Player,P), color(Player), Turn =.. [turn|[Player]] }.

/**
 * size(-Arg:size)
 *
 * Predicate size, gramtica regel voor de size van het spelbord te parsen.
 */
size(S) --> "size:",b, integer(Heigth), " * ", integer(Width), b, { S =.. [size|[Heigth,Width]] }.


/**
 * state(-Arg:state)
 *
 * Predicate state, gramtica regel voor de state van een spel te parsen.
 */
state(S) --> "state:",b, "undecided",b, { S =.. [state,undecided] }.
state(S) --> "state:",b, "won",b,"by",b,nonblanks(X),b, {atom_codes(State,X),color(State), S =.. [state,State] }.


/**
 * orientation(-Arg:orientation)
 *
 * Predicate orientation, gramtica regel voor de orientatie van een spel te parsen.
 */
orientation(S) --> "orientation:",b, nonblanks(H),b, "*",b, nonblanks(V), b, { atom_codes(Hor,H),atom_codes(Ver,V), S =.. [orientation,Hor,Ver] }.

/**
 * b
 *
 * Just a very short way to say skip one or more spaces,tabs or new lines.
 */
b --> blanks.

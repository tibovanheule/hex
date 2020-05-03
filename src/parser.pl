:- module(parser, [parse/1,parseTest/2]).

/** <module> Parser
Behandelt de stream en maakt een Game-datastructur aan.

@author Tibo Vanheule

*/
:- use_module(library(dcg/basics)).
:- use_module(colors).
:- use_module(write).

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
 * gramatica entry point calls perm.
 */
gram(Game) --> perm(_,_,_,_,_,_,Game).

/**
 * perm(-Arg:size,-Arg:turn,-Arg:Number_of_tiles,-Arg:tiles,-Arg:state,-Arg:orientation,-Arg:game)
 *
 * perm zorgt ervoor dat de gramtica door elkaar mag staan. elke permutatie van de lijnen kan zo behandeld worden.
 */
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> size(Size),!, perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game).
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> turn(Turn),!, perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game).
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> tiles(Number_of_tiles,Tiles),!, perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game).
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> state(State),!, perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game).
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> orientation(Or),!, perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game).
perm(Size,Turn,Number_of_tiles,Tiles,State,Or,Game) --> { error(Size,Turn,Number_of_tiles,Tiles,State,Or), Game =.. [game,Size,Turn,Number_of_tiles,Tiles,State,Or]}.

/**
 * error(-Arg:size,-Arg:turn,-Arg:Number_of_tiles,-Arg:tiles,-Arg:state,-Arg:orientation)
 *
 * Bekijkt of alles is geinitialiseerd, zoniet dan is de parsing gefaald en dient er een gepaste foutcode gegeven te worden.
 * controleerd ook de kleuren van de tiles en orienttie.
 */
error(Size,_,_,_,_,_) :- var(Size), write_error("Failed to parse size.").
error(_,Turn,_,_,_,_) :- var(Turn), write_error("Failed to parse turn.").
error(_,_,Number_of_tiles,_,_,_) :- var(Number_of_tiles), write_error("Failed to parse Number_of_tiles.").
error(_,_,_,Tiles,_,_) :- var(Tiles), write_error("Failed to parse Tiles.").
error(_,_,_,_,State,_) :- var(State), write_error("Failed to parse State.").
error(_,_,_,_,_,Or) :- var(Or), write_error("Failed to parse Orientation.").
error(_,_,_,Tiles,_,Or) :- \+ same_color(Tiles,Or), write_error("Found color in tiles not in orientation.").
error(_,_,_,_,_,_) :- true. % geen error

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
same_color(tiles(Tiles),orientation(X,Y)) :- maplist(verify([X,Y]), Tiles).

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
     b,
     {Size =.. [number_of_tiles,S], T =.. [tiles,Tiles] }.

/**
 * tile(-Arg:Int,-Arg:List)
 *
 * Predicate orientation, gramtica regel voor de tegels de parsen
 */
tile(0,L) --> {L = []}.
tile(N,Tiles) -->    b,
    "(",
    nonblank(X), integer(Y),
    ")",
    b,
    "->",
    b,
    nonblanks(P),
    b,
    { N2 is N - 1, atom_codes(Player,P),color_valid(Player),!},
    tile(N2,L),b,
    {
      atom_codes(Kol,[X]),
      char_code('A',Base),
      atom_codes(Kol,Val),
      Koli is Val - Base + 1,
      append([tile(coordinate(Koli/Y),player(Player))],L,Tiles)
     }.

 /**
  * color_valid(-Arg:color)
  *
  * Checks if a color exists, print an error and quit otherwise.
  */
color_valid(Player) :- \+ color(Player), string_concat("Unkown color: ", Player, Out),write_error(Out).
color_valid(_) :- true.

/**
 * turn(-Arg:turn)
 *
 * Predicate turn, gramtica regel voor de beurt van een speler te parsen.
 */
turn(Turn) --> "turn:",b,nonblanks(P),b, { atom_codes(Player,P), color_valid(Player), Turn =.. [turn|[Player]] }.

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
state(S) --> "state:",b, "won",b,"by",b,nonblanks(X),b, {atom_codes(State,X), color_valid(State), S =.. [state,State] }.


/**
 * orientation(-Arg:orientation)
 *
 * Predicate orientation, gramtica regel voor de orientatie van een spel te parsen.
 */
orientation(Or) --> "orientation:",b, nonblanks(H),b, "*",b, nonblanks(V), b, { atom_codes(Hor,H),atom_codes(Ver,V),color_valid(Hor),color_valid(Ver), Or =.. [orientation,Hor,Ver]}.

/**
 * b
 *
 * Just a very short way to say skip one or more spaces,tabs or new lines.
 */
b --> blanks.

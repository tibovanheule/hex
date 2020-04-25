:- module(parser, [parse/1]).
:- use_module(library(dcg/basics)).



parse(Retval) :- phrase_from_stream(gram(A,B,C),user_input),
                 write(A),
                 write(B),
                 write(C).

parse(_) :-
    set_output(user_error),
    write("Failed to parse."),
    halt(4).

gram(P,P2,P3) --> size(P), turn(P2), tiles(P3).

tiles(Size) --> "tiles:",b,integer(S), tile(S,Tiles), {Size =.. [number_tiles|[S]] }.

tile(N,Tiles) --> b, "(", string(Tile), ")",b,"->",b, string(_), N2 is N - 1.

turn(Turn) --> "turn:",b, string(Player),b, { Turn =.. [turn|[Player]] }.

size(S) --> "size:",b, integer(Heigth), " * ", integer(Width),list(_), b, { S =.. [size|[Heigth,Width]] }.

b --> blanks.

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

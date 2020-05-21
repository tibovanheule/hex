:- use_module(parser).
:- use_module(test).
:- use_module(game).
:- use_module(write).
:- use_module(dijkstra).
:- use_module(ai).

/** <module> Main
Deze module is de start entry van het project Hex.

@author Tibo Vanheule

*/

/**
 * main()
 *
 * Entry point for the program, handles arguments and calls all needed modules.
 */
main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).
main(Y) :- member('DIJKSTRA',Y),parse(R),asserta(R),!,get_tiles(T),get_path(4/4, 0/(-1),T,Path,red,true,4),get_dist(4/4, 0/(-1),T,D,red,true,4),write(D),nl,update_board_dijkstra(R,Path,B),write_svg(B,0).
main(Y) :- member('TEST',Y),member('SVG',Y),parse(R),asserta(R),!,test_svg(R).
main(Y) :- member('TEST',Y),parse(R),asserta(R),!,test(R).
main(Y) :- member('SVG',Y),parse(R),asserta(R),!,write_svg(R,0).
main(_) :- parse(R),asserta(R),!, bestmove(R,3,Move),update_board(R,Move,NewBoard),test_write_board(NewBoard).



% wat doet read_string(user_input,"\n","\r",End,Codes) ?
%-------------------
%Uit documenttie
%------------------
% Read a string from Stream, providing functionality similar to split_string/4. The predicate performs the following steps:
%1. Skip all characters that match PadChars
%2. Read up to a character that matches SepChars or end of file
%3. Discard trailing characters that match PadChars from the collected input
%4. Unify String with a string created from the input and Sep with the separator character read. If input was terminated by the end of the input, Sep is unified with -1.
%The predicate read_string/5 called repeatedly on an input until Sep is -1 (end of file) is equivalent to reading the entire file into a string and calling split_string/4, provided that SepChars and PadChars are not partially overlapping.157 Below are some examples:
%------------------
% eigen woorden
%------------------
% leest een strings gedeeld door de seperator '\n' en sla de '\r' over. (support voor windows). De string wordt gemaakt van de user input stream.

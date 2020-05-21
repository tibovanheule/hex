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
main(Y) :- member('TEST',Y),member('SVG',Y),parse(R),asserta(R),!,test_svg(R).
main(Y) :- member('TEST',Y),parse(R),asserta(R),!,test(R).
main(Y) :- member('SVG',Y),parse(R),asserta(R),!,write_svg(R,0).
main(_) :- parse(R),asserta(R),!, bestmove(R,3,Move),update_board(R,Move,NewBoard),write_board(NewBoard).

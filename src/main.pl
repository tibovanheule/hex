main :-
    current_prolog_flag(argv, Argv),
    sort(Argv,ArgvSorted),
    main(ArgvSorted).

% wat doet read_string(user_input,"\n","\r",_,Codes) ?

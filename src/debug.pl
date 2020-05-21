gettttt(PX,PY) :- findall(X/Y, (between(1, 0, X), between(1, 0, Y)), L),write(L).

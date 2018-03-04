p(X,Z) :- q(X,Y), p(Y,Z).
p(X,X).
q(a,b).

append([],Y,Y).
append([X|XS],Y,[X|Z]) :- append(XS,Y,Z).

elem(X,L) :- append(L1,[X|L2],L).
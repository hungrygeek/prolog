xor(0,1).
xor(1,0).

xorlist([],[]).
xorlist([H1|T1],[H2|T2]) :- xor(H1,T1), xorlist(T1,T2).
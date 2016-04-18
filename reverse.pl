tail(T,[]).
tail(T,[H1|T1]) :- tail(H,T1).
 
take([H|T],H,T).
take([H|T],R,[H|S]) :- take(T,R,S).

reverse([],[]).
reverse([H1|T1],[H2|T2]) :-  tail(H1,T2), take([H2|T2],H1,T3), reverse(T1,T3). 
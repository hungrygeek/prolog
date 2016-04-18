

memberoflist(A,[A|T]).
memberoflist(A,[H|T]) :- memberoflist(A,T).

eightqueens([],N).
eightqueens([(C1/R1)|Otherqueens]) :- 
	eightqueens(Otherqueens), 
	memberoflist(R1,[1,2,3,4,5,6,7,8]),
	checkdiagonal((C1/R1),Otherqueens).

checkdiagonal((C1/R1),[]).
checkdiagonal((C1/R1),[(C2/R2),Otherqueens]) :-
	R1 =\= R2,
	C1 - C2 =\= R1 - R2,
	C1 - C2 =\= R2 - R1,
	checkdiagonal((C1/R1),Otherqueens).



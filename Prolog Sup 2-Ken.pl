% 1. negation

?- X=1.
X = 1.

% unify X with 1

?- not(X=1).
false.
% fails because X unifies with 1

?- not(not(X=1)).
true.

% not(X=1) returns false , then we have not(false) which returns true

?- not(not(not(X=1))).
false.
 
% similiar with what happen in not(not(X=1)) , now we have an extra step evaluates not(true)
%wh would returns false

% 2. database

|: qNameColelge(N,C) :- tName(X,N),tCollege(X,C).
|: % user://1 compiled 0.00 sec, 2 clauses
true.

?- qNameColelge(A,B).
A = 'Andrew Rice',
B = 'Churchill' ;
A = 'Alastair Beresford',
B = 'Robinson'.

% 3. database

|: qInfo(Id) :- tName(Id,N),tCollege(Id,C),print(N),print(C).
|: % user://2 compiled 0.00 sec, 2 clauses
true.

?- qInfo(A).
Andrew RiceChurchill
A = acr31 ;
Alastair BeresfordRobinson
A = arb33.

% 4. 

% 5.
minGrade(C) :- tGrade(C,_,3),!,print(3).
minGrade(C) :- tGrade(C,_,2.2),!,print(2.2).
minGrade(C) :- tGrade(C,_,2.1),!,print(2.1).
minGrade(C) :- tGrade(C,_,1),!,print(1).

?- minGrade(acr31).
2.1
true.

% 6.
isFirst(C,Y,V) :- tGrade(C,Y,1),isFirst(C,Y,1),!. 
isFirst(C,Y,V) :-tGrade(C,Y,2.1),isFirst(C,Y,0),!.
isFirst(C,Y,V) :-tGrade(C,Y,2.2),isFirst(C,Y,0),!.
isFirst(C,Y,V) :-tGrade(C,Y,3),isFirst(C,Y,0),!.
countGrade(C,Fg) :- isFirst(C,'IA',A),isFirst(C,'IB',B),isFirst(C,'II',C),Nfg is A+B+C,countGrade(C,Nfg),!.
countGrade(C) :- countGrade(C,0).
% infinite loop here, doesn't cut right?


% 7.

exsitF(C) :- tGrade(C,_,1).

append([],A,A).
append([H|T],A,[H|R]) :- append(T,A,R).

memberoflist(A,[A|T]).
memberoflist(A,[H|T]) :- memberoflist(A,T).

len([],0).
len([_|T],N) :- len(T,M),N is M+1.

countFl(Nl) :- exsitF(C), not(memberoflist(C,Nl)),append(Nl,[C],R),countFl(R).

countF(X) :- countFl(A), len(A,X).

% 8. CountDown

take([H|T],H,T).
take([H|T],R,[H|S]) :- take(T,R,S).

choose(0,L,R,S).
choose(N,[H|T],R,S) :- N > 0, take(S,H,P), M is N-1, choose(M,T,[H|R],P).
choose(N,[H|T],R,S) :- choose(N,T,R,S).

% 9.
eval(division(A,B),C) :- eval(A,A1),eval(B,B1), division(A,B,C).
eval(minus(A,B),C) :- eval(A,A1),eval(B,B1), C is A1-B1.
eval(A,A).

division(0,B,C).
division(A,B,C) :- A > 0,!,A1 is A-B, C1 is C+1, division(A1,B,C1).

%10 Graph Search. Missionaries and Cannibals
% step(MissOnBoat,CannOnBoat)
step(1,0).
step(2,0).
step(1,1).
step(0,1).
step(0,2).
% state(MissOnOneSide,CannOnOneSide,MissOnOtherSide,CannOnOtherSide)
state(3,3,0,0).
state(3,2,0,1).
state(3,1,0,2).
state(3,0,0,3).
state(2,3,1,0).
state(2,2,1,1).
state(2,1,1,2).
state(2,0,1,3).
state(1,3,2,0).
state(1,2,2,1).
state(1,1,2,2).
state(1,0,2,3).
state(0,3,3,0).
state(0,2,3,1).
state(0,1,3,2).
state(0,0,3,3).

transit(state(A,B,C,D),state(E,F,G,H),step(X,Y)) :- A is E + X, B is F + Y, G is C + X, H is D + Y.
issafe(state(A,B,C,D)) :- B =< A, D =< C.

travelsafe(A,A,[]).
travelsafe(A,C,[step(X,Y)|steps]) :- issafe(B), transit(A,B,step(X,Y)),travelsafe(B,C,steps).

% 11. Graph Search Towers of Hanoi

towerofhanoi(N) :- pathhanoi(N, l, r, c).

pathhanoi(0,_,_,_) :- !.
pathhanoi(N,A,B,C) :- N1 is N-1, pathhanoi(N1, A, C, B), move(A, B), pathhanoi(N1, C, B, A).

move(A,B) :- write([move, A, to, B]).

% 12. Umbrella puzzle.

% 13. find min and sort
append([],A,A).
append([H|T],A,[H|R]) :- append(T,A,R).

min([H|T], Min) :- min(T, H, Min).
min([], Min, Min).
min([H|T], Min, Min1) :- ( H < Min ->  min(T, H, Min1); min(T, Min, Min1)).

minsort([],R).
minsort(L,S) :- min(L,X), take(L,X,R), append(S,[X],S1), minsort(R,S1).

% 14. quicksort

qsort([X|Xs], R) :- part(Xs, X, L, H), qsort(L, SL), qsort(H, SH), append(SL, [X|SH], R).
qsort([], []).

part([],_,[],[]).
part([X|Xs], P, [X|Ys], Zs) :- X < P, !, part(Xs, P, Ys, Zs).
part([X|Xs], P, Ys, [X|Zs]) :- part(Xs, P, Ys, Zs).


% 15. 

qsort([X|Xs], R) :- part(Xs, X, L, H, P), qsort(L, SL), qsort(H, SH), append(SL, P, Rt), append(Rt,SH, R).
qsort([], []).

part([],X,[],[],[X]).
part([X|Xs], P, Ys, Zs, [X|Ps]) :- X = P, !, part(Xs, P, Ys, Zs, Ps).
part([X|Xs], P, [X|Ys], Zs, Ps) :- X < P, !, part(Xs, P, Ys, Zs, Ps).
part([X|Xs], P, Ys, [X|Zs], Ps) :- part(Xs, P, Ys, Zs, Ps).

% 16.
this version would be desirable if there are many elements of the same value in the origianl list.

% 17. different list quick sort
qsort([X|Xs]-T0, SL-T6) :- part(Xs-T0, X, L-T2, H-T3), qsort(L-T2, SL-[X|SH]), qsort(H-T5, SH-T6).
qsort(A-A1, A-A1) :- unify_with_occurs_check(A,A1).

% 18. merge sort

merge([],L,L).
merge(L,[],L).
merge([H1|T1],[H2|T2],[H1|T3]) :- H1 =< H2, merge(T1,[H2|T2],T3).
merge([H1|T1],[H2|T2],[H2|T3]) :- H1 > H2, merge([H1|T1],T2,T3).

half([],[],[]).
half([H|T],[H|L1],L2):- half(T,L2,L1).


mergesort([],[]).
mergesort([X],[X]).
mergesort(L,LSorted) :- half(L,L1,L2), mergesort(L1,L1S), mergesort(L2,L2S), merge(L1S,L2S,LSorted).

% 19. Towers of Hanoi

% 20. dutch flag problem
% normal list
scan([],X,[],[],[X]).
scan([X|Xs], [X|R], W, B) :- X = red, !, scan(Xs, R, W, B).
scan([X|Xs], R, [X|W], B) :- X = white !, scan(Xs, R, W, B).
scan([X|Xs], R, W, [X|B]) :- X = blue, !, scan(Xs, S, W, B).

dutchflag([],[])
dutchflag(L,Flag) :- scan(L,R,W,B) , append (R,W,T) , append(T,B,Flag).

% difference list version.

dutchflag(A-A1,A-A1) :- unify_with_occurs_check(A,A1).
dutchflag(L-T,R-T3) :- scan(L-T,R-W,W-B,B-T3).

% 21.
diff(L) :- L in 0..9, all_different(L).
solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y]) :- diff([S,E,N,D,M,O,R,Y]), 
	1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    label([S,E,N,D,M,O,R,Y]).

?- use_module(library(bounds)).
%  library(clp/clp_events) compiled into clp_events 0.00 sec, 9 clauses
% library(bounds) compiled into bounds 0.01 sec, 216 clauses
true.

?- [CryptarithmeticPuzzle].
ERROR: load_files/2: Arguments are not sufficiently instantiated
???

% 22. first digit not zero
diff(L) :- L in 0..9, all_different(L).
solve :- diff([S,E,N,D,M,O,R,Y]), S #\= 0, M #\= 0,
	1000 * S + 100 * E + 10 * N + D + 1000 * M + 100 * O + 10 * R + E #= 10000 * M + 1000 * O + 100 * N + 10 * E + Y,
    label([S,E,N,D,M,O,R,Y]).

% 23. Various base

diff(L,Base) :- L in 0..Base1, Base1 is Base - 1, all_different(L).
solve([S,E,N,D],[M,O,R,E],[M,O,N,E,Y], Base) :- diff([S,E,N,D,M,O,R,Y], Base), 
	pow(Base,3) * S + pow(Base,2) * E + Base * N + D + pow(Base,3) * M + pow(Base,2) * O + Base * R + E is pow(Base,4) * M + pow(Base,3) * O + pow(Base,2) * N + Base * E + Y,
    label([S,E,N,D,M,O,R,Y]).













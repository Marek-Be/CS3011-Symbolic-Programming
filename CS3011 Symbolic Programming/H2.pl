s(String) :- stringAfterTwo(String,B), stringBeforeTwo(String,A), countZero(Y, A), countOne(X,B), Z is X, ZZ is 2*Z, YY is Y, ZZ = YY.

stringAfterTwo([H|T], L) :- H = 2, L = T.
stringAfterTwo([_|T], L) :- stringAfterTwo(T, L).

stringBeforeTwo(X, L) :- reverse(X,Y,[]), stringAfterTwo(Y,L).

countZero(0,[]).
countZero(L+1, [H|T]) :- countZero(L,T), H = 0.
countZero(L, [H|T]) :- countZero(L,T), H \= 0.

countOne(0,[]).
countOne(L+1, [H|T]) :- countOne(L,T), H = 1.
countOne(L, [H|T]) :- countOne(L,T), H \= 1.

reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).

zebraCheck([_,N1,P1,_,N2,P2,_,N3,P3]):-
    Nats1 = [english,spanish,japanese], Pets1 = [jaguar,snail,zebra],
    member(N1, Nats1), delete(Nats1, N1, Nats2),
    member(N2, Nats2), delete(Nats2, N2, Nats3),
    member(N3, Nats3),
    member(P1, Pets1), delete(Pets1, P1, Pets2),
    member(P2, Pets2), delete(Pets2, P2, Pets3),
    member(P3, Pets3).

mkList(0, []) :- true.
mkList(X,[X|T]) :- X \= 0, Y is X-1, mkList(Y,T).

num_split(0, []).
num_split(N, [X | List]) :-
    between(1, N, X),
    plus(X, Y, N),
    num_split(Y, List).

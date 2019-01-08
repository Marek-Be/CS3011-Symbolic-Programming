pterm(null).
pterm(f0(X)) :- pterm(X).
pterm(f1(X)) :- pterm(X).


incr(X,Y) :- X = pterm(null), Y = pterm(f1(null)).
incr(X,Y) :- X = pterm(f0(Z)), Y = pterm(f1(Z)); (X = pterm(f1(A)), Y = pterm(f0(B)), incr(pterm(A),pterm(B))).


legal(pterm(X)) :- legalize(X, f0(null)).
legalize(X, Y) :- X = Y; incr(pterm(Y),pterm(Z)), legalize(X,Z).

incrR(X,Y) :- legal(X), incr(X,Y).


normalize(pterm(P),pterm(Pn)) :- Pn = f0(null), zeroes(P, null).
normalize(pterm(P),pterm(Pn)) :- normalize1(pterm(P),pterm(Pn)).

zeroes(X,Y) :- Y = null, X = f0(null).
zeroes(X,Y) :- X = f0(A), Y = null, zeroes(A, null).

normalize1(pterm(P),pterm(Pn)) :- P = null, Pn = f0(null).
normalize1(pterm(P),pterm(Pn)) :- P = f1(null), Pn = f1(null).
normalize1(pterm(P),pterm(Pn)) :- zeroes(P, null), Pn = null.
normalize1(pterm(P),pterm(Pn)) :- P = f1(X), Pn = f1(Y), normalize1(pterm(X), pterm(Y)).
normalize1(pterm(P),pterm(Pn)) :- P = f0(X), Pn = f0(Y), normalize1(pterm(X), pterm(Y)), X \= null.

% Realized after doing this you could use incr to repeatedly take away
% from X and Y adding into Z. Too lazy to fix.
add(pterm(X), pterm(Y), pterm(Z)) :- add1(X,Y,Z,0). %Last parameter is carry.

%When X and Y have reached the end.
add1(X,Y,Z,C) :- X = f0(null), Y = f0(null), C = 0, Z = f0(null).
add1(X,Y,Z,C) :- X = f0(null), Y = f1(null), C = 0, Z = f1(null).
add1(X,Y,Z,C) :- X = f1(null), Y = f0(null), C = 0, Z = f1(null).
add1(X,Y,Z,C) :- X = f1(null), Y = f1(null), C = 0, Z = f0(f1(null)).
add1(X,Y,Z,C) :- X = f0(null), Y = f0(null), C = 1, Z = f1(null).
add1(X,Y,Z,C) :- X = f0(null), Y = f1(null), C = 1, Z = f0(f1(null)).
add1(X,Y,Z,C) :- X = f1(null), Y = f0(null), C = 1, Z = f0(f1(null)).
add1(X,Y,Z,C) :- X = f1(null), Y = f1(null), C = 1, Z = f1(f1(null)).
%Where X has reached the end.
add1(X,Y,Z,C) :- X = f0(null), Y = f0(A), C = 0, Z = f0(A).
add1(X,Y,Z,C) :- X = f0(null), Y = f1(A), C = 0, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(null), Y = f0(A), C = 0, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(null), Y = f1(B), C = 0, Z = f0(D), add1(f0(null),B,D,1).
add1(X,Y,Z,C) :- X = f0(null), Y = f0(A), C = 1, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(null), Y = f0(B), C = 1, Z = f0(D), add1(f0(null),B,D,1).
add1(X,Y,Z,C) :- X = f0(null), Y = f1(B), C = 1, Z = f0(D), add1(f0(null),B,D,1).
add1(X,Y,Z,C) :- X = f1(null), Y = f1(B), C = 1, Z = f1(D), add1(f0(null),B,D,1).
%Where Y has reached the end.
add1(X,Y,Z,C) :- X = f0(A), Y = f0(null), C = 0, Z = f0(A).
add1(X,Y,Z,C) :- X = f0(A), Y = f1(null), C = 0, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(A), Y = f0(null), C = 0, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(A), Y = f1(null), C = 0, Z = f0(D), add1(A,f0(null),D,1).
add1(X,Y,Z,C) :- X = f0(A), Y = f0(null), C = 1, Z = f1(A).
add1(X,Y,Z,C) :- X = f1(A), Y = f0(null), C = 1, Z = f0(D), add1(A,f0(null),D,1).
add1(X,Y,Z,C) :- X = f0(A), Y = f1(null), C = 1, Z = f0(D), add1(A,f0(null),D,1).
add1(X,Y,Z,C) :- X = f1(A), Y = f1(null), C = 1, Z = f1(D), add1(A,f0(null),D,1).
%Rules for addition.
add1(X,Y,Z,C) :- X = f0(A), Y = f0(B), C = 0, Z = f0(D), add1(A,B,D,0).
add1(X,Y,Z,C) :- X = f0(A), Y = f1(B), C = 0, Z = f1(D), add1(A,B,D,0).
add1(X,Y,Z,C) :- X = f1(A), Y = f0(B), C = 0, Z = f1(D), add1(A,B,D,0).
add1(X,Y,Z,C) :- X = f1(A), Y = f1(B), C = 0, Z = f0(D), add1(A,B,D,1).
add1(X,Y,Z,C) :- X = f0(A), Y = f0(B), C = 1, Z = f1(D), add1(A,B,D,0).
add1(X,Y,Z,C) :- X = f0(A), Y = f1(B), C = 1, Z = f0(D), add1(A,B,D,1).
add1(X,Y,Z,C) :- X = f1(A), Y = f0(B), C = 1, Z = f0(D), add1(A,B,D,1).
add1(X,Y,Z,C) :- X = f1(A), Y = f1(B), C = 1, Z = f1(D), add1(A,B,D,1).


mult(pterm(X),pterm(Y),pterm(Z)) :- (X = f0(null); Y = f0(null)),Z = f0(null).
mult(pterm(X),pterm(Y),pterm(Z)) :- mult1(X,Y,Z,f0(null)).

mult1(X,Y,Z,C) :- (X = null; Y = null), Z = C.
mult1(X,Y,Z,C) :- X = f0(A), mult1(A, f0(Y), Z, C).
mult1(X,Y,Z,C) :- X = f1(A), add(pterm(C), pterm(Y), pterm(B)), mult1(A, f0(Y), Z, B).


revers(pterm(X), pterm(Y)) :- X = null, Y = null.
revers(pterm(X), pterm(Y)) :- (rev(Y,X,pterm(null)); rev(X,Y,pterm(null))), sameLen(Y,X).

findUntil(X, Y) :- X = Y.
findUntil(X, Y) :- X = f1(A), findUntil(A, Y).
findUntil(X, Y) :- X = f0(A), findUntil(A, Y).

rev(X,Y,pterm(A)):- X = null,Y=Y,A=A.
rev(X,Y,pterm(A)):- X = f0(I),findUntil(Y,f0(A)),rev(I, Y, pterm(f0(A))).
rev(X,Y,pterm(A)):- X = f1(I),findUntil(Y,f1(A)),rev(I, Y, pterm(f1(A))).

sameLen(X,Y) :- X=null, Y =null.
sameLen(X,Y) :- X=f0(A), Y =f0(B),sameLen(A,B).
sameLen(X,Y) :- X=f1(A), Y =f0(B),sameLen(A,B).
sameLen(X,Y) :- X=f0(A), Y =f1(B),sameLen(A,B).
sameLen(X,Y) :- X=f1(A), Y =f1(B),sameLen(A,B).


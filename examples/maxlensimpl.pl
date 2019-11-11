len([],o).
len([H|T],s(L)) <- len(T,L).

max(Ls,M) <- max1(Ls,o,M).

max1([],M,M).
max1([H|T],N,M) <-
	le(H,N), max1(T,N,M).
max1([H|T],N,M) <-
	gt(H,N), max1(T,H,M).

le(o,X).
le(s(X),s(Y)) <- le(X,Y).

gt(s(X),o).
gt(s(X),s(Y)) <- gt(X,Y).

maxlen(Ls,M,L) <- max(Ls,M),len(Ls,L).  

<- maxlen(Ls,M,L).
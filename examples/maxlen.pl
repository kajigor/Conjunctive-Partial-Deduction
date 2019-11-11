len([],o).
len([H|T],s(L)) <- len(T,L).

max(Ls,M) <- max1(Ls,o,M).

max1([],M,M).
max1([H|T],N,M) <-
	le(H,N,t), max1(T,N,M).
max1([H|T],N,M) <-
	gt(H,N,t), max1(T,H,M).

le(o,X,t).
le(s(X),o,f).
le(s(X),s(Y),R) <- le(X,Y,R).

gt(s(X),o,t).
gt(o,X,f).
gt(s(X),s(Y),R) <- gt(X,Y,R).

maxlen(Ls,M,L) <- max(Ls,M),len(Ls,L).  

<- maxlen(Ls,M,L).
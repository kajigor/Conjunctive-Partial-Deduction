le(o,X,t).
le(s(X),o,f).
le(s(X),s(Y),R) <- le(X,Y,R).

gt(s(X),o,t).
gt(o,X,f).
gt(s(X),s(Y),R) <- gt(X,Y,R).

<- gt(X,Y,R).

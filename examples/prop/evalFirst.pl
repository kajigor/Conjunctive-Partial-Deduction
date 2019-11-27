eval(St,conj(X,Y),U) <- eval(St,X,V), eval(St,Y,W), and(V,W,U).
eval(St,or(X,Y),U)   <- eval(St,X,V), eval(St,Y,W), or(V,W,U).
eval(St,var(X),U)    <- elem(X,St,U).

elem(o,[H|T],H).
elem(s(N),[H|T],V) <- elem(N,T,V).

nand(f,f,t).
nand(f,t,t).
nand(t,f,t).
nand(t,t,f).

not(A,Na) <- nand(A,A,Na).

or(A,B,C) <- nand(A,A,Aa), nand(B,B,Bb), nand(Aa,Bb,C).

and(A,B,C) <- nand(A,B,Ab), nand(Ab,Ab,C).

<- eval(St,conj(var(a),disj(var(b),var(a))),t).
p([Q2],G).
p([X1,X2|Xs],G) :- p_(X1,X2,G),p([X2|Xs],G).
p([],G).
p_(X1,X2,[pair(B1,B2)|Ys]) :- p__(X1,B1,X2,B2).
p_(X1,X2,[pair(B1,B2)|Ys]) :- p____(X1,B1,B2,Q18,X1,X2,Ys).
p_(X1,X2,[pair(B1,B2)|Ys]) :- p_______(X1,B1,B2,X1,X2,Ys).
p__(s(X___),s(Y_),X2,B2) :- p__(X___,Y_,X2,B2).
p__(z,z,X2,B2) :- p___(X2,B2).
p___(s(X___),s(Y_)) :- p___(X___,Y_).
p___(z,z).
p____(s(Q27),z,B2,Q18,X_____,X2,Ys) :- p_____(X2,B2,Q18,X_____,X2,Ys).
p____(s(X______),s(Y_),B2,Q18,X_____,X2,Ys) :- p____(X______,Y_,B2,Q18,X_____,X2,Ys).
p____(z,s(Q29),B2,Q18,X_____,X2,Ys) :- p______(X2,B2,Q18,X_____,X2,Ys).
p_____(s(Q27_),z,false,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p_____(s(X_________),s(Y_),Q18,X_____,X________,Ys) :- p_____(X_________,Y_,Q18,X_____,X________,Ys).
p_____(z,s(Q29),false,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p_____(z,z,true,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p______(s(Q27),z,false,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p______(s(X_________),s(Y_),Q18,X_____,X________,Ys) :- p______(X_________,Y_,Q18,X_____,X________,Ys).
p______(z,s(Q29_),false,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p______(z,z,true,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p_______(s(X______),s(Y_),B2,X_____,X2,Ys) :- p_______(X______,Y_,B2,X_____,X2,Ys).
p_______(z,z,B2,X_____,X2,Ys) :- p________(X2,B2,X_____,X2,Ys).
p________(s(Q27),z,X_____,X________,Ys) :- p_(X_____,X________,Ys).
p________(s(X_________),s(Y_),X_____,X________,Ys) :- p________(X_________,Y_,X_____,X________,Ys).
p________(z,s(Q29),X_____,X________,Ys) :- p_(X_____,X________,Ys).
<- p(C,G).
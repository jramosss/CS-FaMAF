progenitor(X,Y) :- 
padre(X,Y) :- hombre(X),progenitor(X,Y)
madre(X,Y) :- mujer(X),progenitor(X,Y)
hijo(X,Y) :- hombre(X),(padre(Y,X);madre(Y,X))
abuelo(X,Y) :- hombre(X),padre(X,Y),padre(Y,_)
abuela(X,Y) :- mujer(X),madre(X,Y),madre(Y,_)
suegro(X,Y) :- hombre(X),
novio(X,Y) :- hombre(X),suegro(X,Y)
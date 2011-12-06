/* Logik f. Inf. 
   Uebung 5, Aufgabe 3. 
   Bernhard Haeussner */

/* Eine atomare Formel enthaelt nur das Atom */

boolean_atoms(A, [A]) :-
  boolean_formula_is_atomic(A), !.

/* Alle anderen Formeln enthalten die 
   Atome ihrer Teilformeln */

boolean_atoms( (A,B), Cs ) :- 
  boolean_atoms(A,CAs),
  boolean_atoms(B, CBs),
  append(CBs,CAs,Css), 
  sort(Css,Cs).

boolean_atoms( (A;B), Cs ) :- 
  boolean_atoms(A,CAs),
  boolean_atoms(B, CBs),
  append(CBs,CAs,Css),
  sort(Css,Cs).

boolean_atoms( -A, Cs ) :- 
  boolean_atoms(A,CAs),
  append([],CAs,Css),
  sort(Css,Cs).

/* Praedikat aus dem Script */

boolean_formula_is_atomic(A) :-
  \+ ( functor(A, F, N), member(F:N, ['-':1, ',':2, ';':2]) ).




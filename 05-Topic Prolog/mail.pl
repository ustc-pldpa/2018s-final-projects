some_element_right([A|X],[A|Y]).
some_emement_right([A|X],[B|Y]):-some element right(X,Y).
letter(X,Y):-
   permutation(X,Y),
   not(some_element_right(X,Y)).


woman(mary).
woman(cindy).
woman(monica).

friend(mary, cindy).

friend(A, B) :- friend(B, A).

father(tom, mary).
father(tom, cindy).

sister(A, B) :-
	woman(A),
	woman(B),
	father(C, A),
	father(C, B).

noon.

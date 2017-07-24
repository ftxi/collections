
fib(0, 0).
fib(1, 1).

fib(A, B) :-
	A > 0,
	B is C + D,
	A1 is A - 1,
	A2 is A - 2,
	fib(A1, C),
	fib(A2, D).

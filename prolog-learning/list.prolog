
append([], List, List).
append([Head|Tail], List, [Head|List']) :-
    append(Tail, List, List') .

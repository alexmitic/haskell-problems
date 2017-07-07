%%%%%%% Database %%%%%%%%
person("A").
person("B").
person("C").
person("D").
person("F").

knows("A", "B").
knows("A", "C").
knows("B", "C").
knows("D", "F").
%%%%%%%%%%%%%%%%%%%%%%%%%

knowEachOther(A, B) :- knows(A, B); knows(B, A).

possibleConsp([], _).
possibleConsp([Head|Tail], X) :- person(X), \+ knowEachOther(Head, X), possibleConsp(Tail, X).

notPartOfList([], _).
notPartOfList([Head|Tail], X) :- person(X), \+ Head == X, notPartOfList(Tail, X). 
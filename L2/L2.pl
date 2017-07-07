person("A").
person("B").
person("C").
person("D").
person("F").

knows("A", "B").
knows("A", "C").
knows("B", "C").
knows("D", "F").   

knowEachOther(A, B) :- knows(A, B); knows(B, A).

isSpider(S) :- person(S), bagof(Q, knowEachOther(Q, S), P), 
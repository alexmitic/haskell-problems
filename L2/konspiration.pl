%%%%%%% Database %%%%%%%%
person(s).
person(c1).
person(c2).
person(o1).
person(o2).

knows(o1, s).
knows(s, c1).
knows(s, c2).

knows(c1, o1).
knows(c2, o2).
%%%%%%%%%%%%%%%%%%%%%%%%%

knowEachOther(A, B) :- knows(A, B); knows(B, A). % Friendships are symmetric

spider(S) :- person(S), isSpider(S).

isSpider(S) :- 
        bagof(Friend, knowEachOther(S, Friend), P), 
        bagof(X, person(X), Peeps),
        subtract(Peeps, [S|P], NonF), 
        possibleConsp(P, [], NonF, K),
        subtract(Peeps, [S|K], Norms),
        knowsOne(Norms, K), !. 

possibleConsp([Head|Tail], K, NonF, Kprime) :- 
                    \+ knowsOneFromList(Head, K), % Check if head dosn't know the other consps
                    bagof(X, knowEachOther(Head, X), Friends), % Find all friends of the consps
                    subtract(Tail, Friends, Pprime), % Remove consps friends from P as they can't be consps 
                    append(Pprime, [Head|K], Test), % Add all potential consps and current consps together
                    knowsOne(NonF, Test), % Check if everyone outside of potential consps and consps knows alleast one from K or P 
                    possibleConsp(Pprime, [Head|K], NonF, Kprime).

possibleConsp([_|Tail], K, NonF, Kprime) :- possibleConsp(Tail, K, NonF, Kprime). % If not consp skip it 

possibleConsp([], K, _, K) :- !.


% Check if everyone from first list knows atleast one form second list 
knowsOne([Head|Tail], List) :- knowsOneFromList(Head, List), knowsOne(Tail, List), !.
knowsOne([], _) :- !.

knowsOneFromList(X, List) :- member(Y, List), knowEachOther(X, Y), !.  
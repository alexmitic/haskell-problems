
knowEachOther(A, B) :- knows(A, B); knows(B, A). % Friendships are symmetric

spider(S) :- person(S), isSpider(S).

isSpider(S) :- 
        bagof(Friend, knowEachOther(S, Friend), SFriends), 
        bagof(X, person(X), AllPeople),
        allSubsets(SFriends, P),
        subtract(AllPeople, [S|P], NonF), 
        possibleConsp(P, [], NonF, K),
        subtract(AllPeople, [S|K], Norms),
        knowsOne(Norms, K), !.


possibleConsp([Head|Tail], K, NonF, Kprime) :-
                    knowsOneFromList(Head, K), % Check if head dosn't know the other consps
                    possibleConsp(Tail, K, NonF, Kprime). % If not consp skip it 

possibleConsp([Head|Tail], K, NonF, Kprime) :- 
                    \+ knowsOneFromList(Head, K), % Check if head dosn't know the other consps
                    bagof(X, knowEachOther(Head, X), Friends), % Find all friends of the consps
                    subtract(Tail, Friends, Pprime), % Remove consps friends from P as they can't be consps 
                    append(Pprime, [Head|K], Test), % Add all potential consps and current consps together
                    knowsOne(NonF, Test), % Check if everyone outside of potential consps and consps knows alleast one from K or P 
                    possibleConsp(Pprime, [Head|K], NonF, Kprime).

possibleConsp([], K, _, K) :- !.

possibleConsp([], [], _, _) :- fail.


% Check if everyone from first list knows atleast one form second list 
knowsOne([Head|Tail], List) :- knowsOneFromList(Head, List), knowsOne(Tail, List), !.
knowsOne([], _) :- !.

knowsOneFromList(X, List) :- member(Y, List), knowEachOther(X, Y).  

allSubsets([], []). % Basecase. Empty list is subset of empyt list

allSubsets([Head|Tail], [Head|SubTail]) :- allSubsets(Tail, SubTail). % If first element from sublist matches first from list continue with rest
allSubsets([_|Tail], SubList) :- allSubsets(Tail, SubList). % If firsts don't match skip first from list and keep sublist

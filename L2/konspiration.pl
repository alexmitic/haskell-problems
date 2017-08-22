%% person(ada).
%% person(beda).
%% person(calle).
%% knows(ada,beda).
%% knows(ada,calle).
%% knows(beda,calle).

knowEachOther(A, B) :- knows(A, B); knows(B, A). % Friendships are symmetric

spider(S) :- person(S), isSpider(S).

isSpider(S) :- 
        findall(Friend, knowEachOther(S, Friend), P), 
        findall(X, person(X), Peeps),
        possibleConsp(P, [], Peeps), !.


possibleConsp([], K, Peeps) :- 
                    \+ findNonFriends(K, Peeps).

possibleConsp([Head|Tail], K, Peeps) :- 
                    findall(X, knowEachOther(Head, X), Friends), % Find all friends of the consp
                    subtract(Tail, Friends, Pprime), % Remove consps friends from P as they can't be consps 
                    append(Pprime, [Head|K], Test), % Add all potential consps and current consps together
                    \+ findNonFriends(Test, Peeps),
                    possibleConsp(Pprime, [Head|K], Peeps).

possibleConsp([_|Tail], K, Peeps) :-
                    possibleConsp(Tail, K, Peeps). % Can't be a consp so skip it 

knowsOneFromList(X, List) :- member(Y, List), knowEachOther(X, Y), !.  

findNonFriends(List, Peeps) :- subtract(Peeps, List, NonF), 
                    member(Y, NonF),
                    \+ knowsOneFromList(Y, List).
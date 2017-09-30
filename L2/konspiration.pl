% Code by Aleksandar Mitic and Oskar Nehlin

knowEachOther(A, B) :- knows(A, B); knows(B, A). % Friendships are symmetric

spider(S) :- person(S), isSpider(S). % For all people check if they can be a spider

isSpider(S) :- 
        findall(Friend, knowEachOther(S, Friend), P), % Get all friends (possible conspirators) of current spider
        findall(X, person(X), Peeps), % A list of all people in database
        possibleConsp(P, [], Peeps), !. % Check if S and be a spider

% If P is empty, check that the generated list of conspirators is a vaild one.
possibleConsp([], K, Peeps) :- 
                    \+ findNonFriends(K, Peeps).

% Case 1:
% Head is a conspirator.
possibleConsp([Head|Tail], K, Peeps) :- 
                    findall(X, knowEachOther(Head, X), Friends), % Find all friends of the consp
                    subtract(Tail, Friends, Pprime), % Remove consps friends from P as they can't be consps 
                    append(Pprime, [Head|K], Test), % Add all potential consps and current consps together
                    \+ findNonFriends(Test, Peeps), % Check at everyone outside of P and K knows atleat one in K or P
                    possibleConsp(Pprime, [Head|K], Peeps).

% Case 2:
% Head is not a conspirator
possibleConsp([_|Tail], K, Peeps) :-
                    possibleConsp(Tail, K, Peeps). % Can't be a consp so skip it 

% Check that X knows atleast one from list
knowsOneFromList(X, List) :- member(Y, List), knowEachOther(X, Y), !.  

% Check that everyone that is not part of list knows atleast one form list
findNonFriends(List, Peeps) :- subtract(Peeps, List, NonF), 
                    member(Y, NonF),
                    \+ knowsOneFromList(Y, List).
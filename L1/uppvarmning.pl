% Fibonacci

% Basecases
fib(0, 0):- !.
fib(1, 1):- !.

% If not basecases send to helper with third Fib number as start
fib(N, F) :- N1 is N - 2, fib(1, 1, N1, F).

fib(_, F, 0, F) :- !. % Basecase for helper

% Otherwise calc Fib numbers linearly until finding the correct           
fib(A, B, N, F) :- 
        N > 0, 
        NewVal is A + B, N1 is N - 1, 
        fib(B, NewVal, N1, F). 


% Rovarsprak

% Basecases
rovarsprak([], []) :- !. 

rovarsprak([Head|TailText], [Head|TailRov]) :-  % If first is the same
        member(Head, [97, 101, 111, 117, 121]), % And is a vokal
        rovarsprak(TailText, TailRov).          % Check rest 

rovarsprak([Head|TailText], [Head, 111, Head | TailRov]) :- % If first in text is konsonant
        rovarsprak(TailText, TailRov).
                        

% Medellangd

medellangd(List, AvgLength) :- sum(List, Chars, 0, Words, 0), AvgLength is Chars / Words, !. 
    
% Helper that calculates number of words and number of letters

sum([], Chars, Chars, Words, Words). % If calculated num words and letter equals the given input 

sum([Head|Tail], Chars, CharCounter, Words, WordCounter) :- % If letter, increment CharCounter
        char_code(Letter, Head),
        char_type(Letter, alpha),
        IncCh is CharCounter + 1, 
        sum(Tail, Chars, IncCh, Words, WordCounter).

sum([Head|Tail], Chars, CharCounter, Words, WordCounter) :- % If not letter, increment WordCounter
        char_code(Letter, Head),
        not(char_type(Letter, alpha)),
        IncWords is WordCounter + 1, 
        sum(Tail, Chars, CharCounter, Words, IncWords).
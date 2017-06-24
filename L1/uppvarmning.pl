% Fibonacci

% Basecases
fib(0, 0).
fib(1, 1).

% If not basecases send to helper with third Fib number as start
fib(N, F) :- N1 is N - 2, fib(1, 1, N1, F).

fib(_, F, 0, F). % Basecase for helper

% Otherwise calc Fib numbers linearly until finding the correct           
fib(A, B, N, F) :- 
        N > 0, 
        NewVal is A + B, N1 is N - 1, 
        fib(B, NewVal, N1, F). 


% Rovarsprak

% Basecases
rovarsprak([], []). 

rovarsprak([Head|TailText], [Head|TailRov]) :-  % If first is the same
        member(Head, [97, 101, 111, 117, 121]), % And is a vokal
        rovarsprak(TailText, TailRov).          % Check rest 

rovarsprak([Head|TailText], [Head, 111, Head | TailRov]) :- % If first in text is konsonant
        rovarsprak(TailText, TailRov).
                        

% Medellangd


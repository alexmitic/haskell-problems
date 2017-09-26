% Code written by Oskar Nehlin 9603035214 and Aleksandar Mitic 9607278737

% Fibonacci
% Basecases
fib(0, 0):- !.
fib(1, 1):- !.

% If not basecases send to helper with third Fib number as start
fib(N, F) :- LeftToCalculate is N - 2, fib(1, 1, LeftToCalculate, F).
fib(_, F, 0, F) :- !. % Basecase for helper

% Otherwise calc Fib numbers linearly until finding the correct
fib(SecondLatest, Latest, LeftToCalculate, F) :-
        LeftToCalculate > 0,
        NewLatest is Latest + SecondLatest,
        LeftToCalculate1 is LeftToCalculate - 1,
        fib(Latest, NewLatest, LeftToCalculate1, F).

% Rovarsprak
% Basecases
rovarsprak([], []) :- !.

rovarsprak([Head|TailText], [Head|TailRov]) :-  % If first is the same
        member(Head, [97, 101, 105, 111, 117, 121]), % And is a vocal
        rovarsprak(TailText, TailRov), !.          % Check rest, cut because if this is true we don't need to check other possibilities
                                                    % that would give us wrogn answers since the second function accepts both vocals
                                                    % and consonant
rovarsprak([Head|TailText], [Head, 111, Head | TailRov]) :- % If first in text is konsonant
        rovarsprak(TailText, TailRov), !.

% Medellangd
medellangd(List, AvgLength) :- sum(List, Letters, Words), AvgLength is Letters / Words, !.

% Helper that calculates number of words and number of letters

sum([], 0, 0). % Empty list equals zero words and letters

sum([Head|[]], 1, 1) :- % If list only contains one letter it also has one word
        char_code(Letter, Head), %convert ASCII value to char
        char_type(Letter, alpha). %check if char is a letter

sum([Head, Second|Tail], Letters, Words) :- % If second is not letter and head is letter we have one word and one letter
        char_code(LetterSecond, Second),
        not(char_type(LetterSecond, alpha)),
        char_code(LetterFirst, Head),
        char_type(LetterFirst, alpha),
        sum(Tail, Letters1, Words1), %recursive call that "returns" new values for number of letters and words.
        Letters is Letters1 + 1, %add 1 since this function call has confirmed a word and a letter exists together with the recursion result
        Words is Words1 + 1.

sum([Head|Tail], Letters, Words) :- % If head letter, words stay the same, letter increments
        char_code(Letter, Head),
        char_type(Letter, alpha),
        sum(Tail, Letters1, Words1),
        Letters is Letters1 + 1,
        Words is Words1.

sum([_|Tail], Letters, Words) :- % If none of the above continue with next part of list
        sum(Tail, Letters, Words).
% Skyffla
% Basecases
skyffla([], []).

skyffla([Head|Tail], Skyfflad) :-
        everyOther([Head|Tail], Result), % Get everyother
        everyOther(Tail, Rest), % Get what is remaining
        skyffla(Rest, RqRes), % Recursivly skyffla the remiander
        append(Result, RqRes, Skyfflad). % Put together lists

everyOther([], []) :- !.

everyOther([Head|[]], [Head|[]]) :- !. %always keep the last element

everyOther([Head, _| Tail], [Head|TailFinal]) :- %skip every other element
        everyOther(Tail, TailFinal). %recursivly do the rest of the list
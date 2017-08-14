% I den här databasen kan bara "1" vara spindeln i nätet.  Mängden
% konspiratörer kan bestå av antingen personerna {2,4,6}, eller
% personerna {3,5,7}.
%
% Strukturen på nätverket finns förklarad nedan.

person(1).
person(2).
person(3).
person(4).
person(5).
person(6).
person(7).
person(8).
person(9).
person(10).
person(11).
person(12).
person(13).

% Person 1 känner personerna 2-7.
knows(1,2).
knows(1,3).
knows(1,4).
knows(1,5).
knows(1,6).
knows(1,7).

% Bekantskapsförhållande för person 2-7 bilder en cykel (2 känner 3, 3
% känner 4, etc upp till 7 som känner 2).
knows(2,3).
knows(3,4).
knows(4,5).
knows(5,6).
knows(6,7).
knows(7,2).

% Två bekanta från 2-7 har en gemensam bekant som ingen annan känner.
% T.ex. har 2 och 3 en gemensam bekant 9,
%           3 och 4 en gemensam bekant 10,
% etc
knows(2,8).
knows(2,9).
knows(3,9).
knows(3,10).
knows(4,10).
knows(4,11).
knows(5,11).
knows(5,12).
knows(6,12).
knows(6,13).
knows(7,13).
knows(7,8).

% I den här databasen kan alla tre personerna vara spindeln i nätet.
% Observera att spider(X) bara ska generera varje person en gång -- om
% man inte gör rätt är det lätt gjort att man får varje person två
% gånger i det här exemplet.
%
% Mer allmänt gäller att i en databas där alla känner alla kan vem som
% helst vara spindeln, och vilken annan person som helst vara en ensam
% konspiratör.  Alla de återstående personerna känner ju den ensamma
% konspiratören, så alla villkor är uppfyllda.
person(ada).
person(beda).
person(calle).
knows(ada,beda).
knows(ada,calle).
knows(beda,calle).

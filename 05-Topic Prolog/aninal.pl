:-dynamic xpositive/2.
:-dynamic xnegative/2.

run:-
   animal_is(X),!,
   write('Your animal may be a(an)'),
   write(X),nl,clear_facts.

run:-
   nl,
   write('Unable to determine what'),
   write('your animal is.'),clear_facts.

positive(X,Y):-
   xpositive(X,Y),!.
positive(X,Y):-
   not(xnegative(X,Y)),
   ask(X,Y).
negative(X,Y):-
   xnegative(X,Y),!.
negative(X,Y):-
   not(xpositive(X,Y)),
   ask(X,Y).
ask(X,Y):-
   write(X),
   write(' it '),
   write(Y),
   nl,
   readln(Reply),
   remember(X,Y,Reply).
remember(X,Y,[y|_]):-
   asserta(xpositive(X,Y)).
remember(X,Y,[n|_]):-
   asserta(xnegative(X,Y)),
   fail.

clear_facts:-
   retract(xpositive(_,_)),
   fail.
clear_facts:-
   retract(xnefative(_,_)),
   fail.
clear_facts:-
   nl,
   write('Space bar to Exit'),
   readln(_).

animal_is(cheetah):-
   it_is(carnivore),
   positive(has,tawny_color),
   positive(has,black_spots).
animal_is(tiger):-
   it_is(carnivore),
   positive(has,tawny_color),
   positive(has,black_stripes).
animal_is(giraffe):-
   it_is(ungulate),
   positive(has,long_neck),
   positive(has,long_legs),
   positive(has,dark_spots).
animal_is(zebra):-
   it_is(ungulate),
   positive(has,black_stripes).
animal_is(ostrich):-
   it_is(bird),
   negative(does,fly),
   positive(has,long_neck),
   positive(has,long_legs),
   positive(has,black_and_white_color).
animal_is(penguin):-
   it_is(bird),
   negative(does,fly),
   negative(does,swim),
   positive(has,black_and_white_color).
animal_is(tiger):-
   it_is(carnivore),
   positive(has,tawny_color),
   positive(has,black_stripes).
animal_is(tiger):-
   it_is(bird),
   positive(has,fly_well).

it_is(mammanl):-
   positive(has,hair).
it_is(mammanl):-
   positive(has,give_milk).
it_is(bird):-
   positive(has,feathers).
it_is(bird):-
   positive(does,fly),
   positive(does,lay_eggs).
it_is(carnivore):-
   positive(does,eat_meat).
it_is(carnivore):-
   it_is(mammal),
   positive(has,pointed_teeth),
   positive(has,claws),
   positive(has,forward_eyes).
it_is(ungulate):-
   it_is(mammal),
   positive(has,hooves).
it_is(ungulate):-
   it_is(mammal),
   positive(does,chew_cud).

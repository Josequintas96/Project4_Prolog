:- dynamic person/2.
:- dynamic family/2. 

available(X, male):-
    free(X).
available(X, female):-
    free(X).



sex(male).
sex(female).

% X is Y gender
% romeo is a male
gender(romeo, male).
gender(martin, male).
gender(tenorio, male).
gender(domingez, male).
gender(megaman, male).
gender(julia, female).
gender(amanda, female).
gender(falcia, female).

% for L to be family, l MUST BE A LIST
is_family(L):-is_list(L).

% On case of marriage, X must be perrson 1 family, Y must be person 2 family, combine resuulut in new family of name V
marriage(X,Y,V) :- atom(V), is_family(X), is_family(Y), X \== Y, append(X,Y,Z), asserta(family(V,Z)).

age(X):-integer(X).

birth(T,T,T) :- age(T).

is_person(X,Y) :- atom(X), sex(Y), asserta(person(X,Y)).

loves(pig, fish).
% pig love  fish


loves(romeo,julia).
loves(romeo, amanda) :- free(romeo).
% Romeo loves Amanda if romeo is free


loves(julia, amanda) :- write('if Romeo die').
% Julia loves Amanda then Romeo is death


date(X, Y) :- free(X), free(Y).
% X date Y if X and Y are free

free(amanda) :- write('amanda is free') .
% free(romeo) :- write('romeo is free') .

% is X free to date
free(pedro) :- write('pedro is free') .
free(martin) :- write('martin is free') .
free(tenorio).
free(domingez).
free(falacia).
free(megaman).


% romeo prent is alfonso
% X parrent is Y
parent(romeo, alfonso).
parent(romeo, maria).
parent(julia, beneric).
parent(julia, vanessa).
parent(amanda, beneric).
parent(amanda, vanessa).
parent(vanessa, pedro).
parent(vanessa, laura).

% the parents of X  are Y and Z
parents(X,Y,Z) :- parent(X,Y), parent(X,Z), Z \== Y.

% grandparent of X must be the parant of the parent of actual X
grandparent(X,Y) :- parent(X, Z), parent(Z,Y).

% X and Y are brother or sister if PARENT OF X ARE SAME AS PARENT OF Y
sister_brother(X,Y) :- parent(X, Z), parent(Y, Z), format('the parent is ~w', [Z]).



loves2(X,fish):-atomic(X).

person(_, _) :- false().
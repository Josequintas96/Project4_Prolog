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
free(pedro) :- write('pedro is free') .
free(martin) :- write('martin is free') .
free(tenorio).
free(domingez).
free(falacia).
free(megaman).



loves2(X,fish):-atomic(X).
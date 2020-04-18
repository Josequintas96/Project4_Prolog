:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
% iplus
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

test(typeExp_iplus_T, [true(T == int)]) :-
typeExp(iplus(int, int), T).

% iminus
test(typeExp_iminus) :- 
    typeExp(iminus(int,int), int).
test(typeExp_iminus) :- 
    typeExp(iminus(20,18), 2).

% this test should fail
test(typeExp_iminus_F, [fail]) :-
    typeExp(iminus(int, int), float).

test(typeExp_iminus_T, [true(T == int)]) :-
typeExp(iminus(int, int), T).

% & imultiply
%test(typeExp_imultiply) :- 
%    typeExp(imultiply(int,int), float).

% this test should fail
%test(typeExp_imultiply_F, [fail]) :-
%    typeExp(imultiply(int, int), int).

%test(typeExp_imultiply_T, [true(T == float)]) :-
%typeExp(imultiply(int, int), T).

% idivide
%test(typeExp_idivide) :- 
%    typeExp(idivide(int,int), float).

% this test should fail
%test(typeExp_idivide_F, [fail]) :-
%    typeExp(idivide(int, int), int).

% test(typeExp_idivide_T, [true(T == float)]) :-
% typeExp(idivide(int, int), T).

% ilessthan
% test(typeExp_ilessthan) :- 
%    typeExp(ilessthan(int,int), bool).

% this test should fail
%test(typeExp_ilessthan_F, [fail]) :-
%    typeExp(ilessthan(int, int), int).

% this test should fail
%test(typeExp_ilessthan_F, [fail]) :-
%    typeExp(ilessthan(int, int), float).

% fplus
test(typeExp_fplus) :- 
    typeExp(fplus(float,float), float).

% this test should fail
test(typeExp_fplus_F, [fail]) :-
    typeExp(fplus(float, float), int).

test(typeExp_fplus_T, [true(T == float)]) :-
typeExp(fplus(float, float), T).

% fminus
test(typeExp_fminus) :- 
    typeExp(fminus(float,float), float).

% this test should fail
test(typeExp_fminus_F, [fail]) :-
    typeExp(fminus(float, float), int).

test(typeExp_fminus_T, [true(T == float)]) :-
typeExp(fminus(float, float), T).

% tests for typeExp
test(typeExp_fToInt) :- 
typeExp(fToInt(float), int).

% tests for typeExp
test(typeExp_iToFloat) :- 
typeExp(iToFloat(int), float).



% NEW :- tests for typeExp_types
% typeExp(X, int) :- integer(X).

test(typeExp_Integer) :- 
    typeExp(5, int).

test(typeExp_Integer_F, [fail]) :- 
    typeExp(15.9, int).

test(typeExp_Float) :- 
    typeExp(9.5, float).

test(typeExp_Float_F, [fail]) :- 
    typeExp(19, float).

test(typeExp_Bool) :- 
    typeExp(9<5, bool).

test(typeExp_Bool) :- 
    typeExp(9==5, bool).

%test(typeExp_Bool) :- 
 %   typeExp(true && false, bool).

%test(typeExp_Bool, [fail]) :- 
%    typeExp(false || true, bool).

test(typeExp_Bool_S, [fail]) :- 
    typeExp(true, float).


% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% test for statement with state cleaning
% test(typeStatement_gvar, [nondet, true(T == float)]) :- % should succeed with T=int
%     deleteGVars(), /* clean up variables */
%     typeStatement(gvLet(v, T, ), unit),
%     assertion(X == int), assertion( Y == int), % make sure the types are int
%     gvar(v, int). % make sure the global variable is defined

test(typeStatement_gvar, [nondet, true(T == int)]) :- 
    deleteGVars(), 
    typeStatement(gvLet(v, T, 9+9), unit),
    gvar(v, int). 


% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

% haskell int ->int ->int
% ifstatement ;=> Cond -> trueB -> falseB ->Resultt

test(simple_if, [nondet]) :-
    typeStatement( if(true, [3], [4]), T),
    assertion(T==int).

test(simple_if, [nondet]) :-
    typeStatement( if(44.1 < 44.2, [hello], [bye]), T),
    assertion(T==atom).

test(simple_if, [nondet], [fail]) :-
    typeStatement( if(44.1 >= 44.2, [44.4], [55.5]), T),
    assertion(T==atom).


%conditional test
% typeStatement(cos(X), T) :- is_a_number(X), typeExp(X, T), is_a_number(T).
%test(simple_cos_I, [nondet]) :-
%    typeStatement( cos(99), T),
%    assertion(T==int).

test(simple_cos_I_F, [nondet]) :-
   typeStatement( cos(91.4), T),
   assertion(T==float).

test(simple_sin, [nondet]) :-
   typeStatement( sin(0.2), T),
   assertion(T==float).

test(simple_sin_F_F, [nondet], [fail]) :-
    typeStatement( sin(91), T).

:-end_tests(typeInf).

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
    typeExp(iminus(20,18), int).

% this test should fail
test(typeExp_iminus_F, [fail]) :-
    typeExp(iminus(int, int), float).

test(typeExp_iminus_T, [true(T == int)]) :-
typeExp(iminus(int, int), T).




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

test(typeStatement_operation1, [nondet, true(T == float)]) :- 
    deleteGVars(), 
    typeStatement(gvLet(v, T, 9.1+9.2), unit),
    gvar(v, int). 

test(typeStatement_operation2, [nondet, true(T == bool)]) :- 
    deleteGVars(), 
    typeStatement(gvLet(v, T, true), unit),
    gvar(v, bool). 

test(typeStatement_operation3, [nondet, true(T == bool)]) :- 
    deleteGVars(), 
    typeStatement(gvLet(v, T, 5==5), unit),
    gvar(v, bool). 
    
test(typeStatement_operation4, [nondet, true(T == float)]) :- 
    deleteGVars(),
    typeStatement(if(>(float,float), [fmultiply(float,float)], [fminus(float, float)]),T).

test(typeStatement_variable, [nondet, true(T2 == float)]) :- 
    deleteGVars(), 
    typeStatement(gvLet(v0, T, 9.1+10.0), unit),
    typeStatement(gvLet(v, T2, v0), unit),
    gvar(v, float). 
    
test(typeStatement_variable, [nondet, true(T == int)]) :- 
    deleteGVars(), 
    typeStatement(gfunc(name, [x, y, z], T, 2+2), unit),
    gvar(name,[x, y, z]). 

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


% typeStatement([(gvLet(a0, T, a1),unit)],where([(l_Let_in(a0, T, 2+2),unit)]), unit).


% haskell int ->int ->int
% ifstatement ;=> Cond -> trueB -> falseB ->Resultt

test(simple_if, [nondet]) :-
    typeStatement( if(true, [3], [4]), T),
    assertion(T==int).

test(simple_if, [nondet]) :-
    typeStatement(gvLet(hello, T, 2+2),unit),
    typeStatement(gvLet(bye, T, 2+2),unit),
    typeStatement( if(44.1 < 44.2, [hello], [bye]), T),
    assertion(T==int).

test(simple_if, [nondet], [fail]) :-
    typeStatement( if(44.1 >= 44.2, [44.4], [55.5]), T),
    assertion(T==atom).


%conditional test
% typeStatement(cos(X), T) :- is_a_number(X), typeExp(X, T), is_a_number(T).
test(simple_cos_I, [nondet]) :-
   typeStatement( cos(99.1), T),
   assertion(T==float).


test(simple_cos_I_F, [nondet]) :-
   typeStatement( cos(91.4), T),
   assertion(T==float).

test(simple_sin, [nondet]) :-
   typeStatement( sin(0.2), T),
   assertion(T==float).

test(simple_sin_F_F, [nondet], [fail]) :-
    typeStatement( sin(91), T).

test(global_do, [nondet]) :-
    typeStatement(do([ gvLet(c, T, iminus(3, 2)), gvLet(v, T2, iplus(8, 4))],T1), unit),
    assertion(T1==unit), assertion(T2 == int), assertion(T == int).

test(extend_do, [nondet]) :-
    typeStatement(do([ gvLet(c, T, iminus(3, 2)), l_Let_in(v, T2, iplus(8, 4))],T1), unit),
    assertion(T1==unit),assertion(T==int), assertion(T2==int).

test(extend_do, [nondet]) :-
    typeStatement(do([ gvLet(c, T, iminus(3, 2)), l_Let_in(v, T2, iplus(8, 4))],T1), unit),
    assertion(T1==unit),assertion(T==int), assertion(T2==int).

test(simple_CODE, [nondet]) :-
    typeCode([ gvLet(c, T, fminus(3.1, 2.1)), (2+2)], T3),
    assertion(T3==int).


test(global_CODE, [nondet]) :-
    typeCode([ gvLet(c, T, fminus(3.1, 2.1))], T3),
    assertion(T3==unit).

test(extend_CODE, [nondet]) :-
    typeCode([ gvLet(c, T, fminus(3.1, 2.1)), (2+2), gvLet(c, T2, iminus(3, 9990)), (2<2)], T3),
    assertion(T3==bool),assertion(T==float), assertion(T2==int).


test(infer_1, [nondet]) :-
    infer([
    if(>(float,float), [iplus(int,int)], [iminus(int,int)])], Ret),
    assertion(Ret==int).


%expressions as statements
test(exprStat, [nondet]) :-
    infer([
        int,
        float,
        bool
        ], Ret),
        assertion(Ret==bool).
        
 %test if statements
test(ifStat, [nondet]) :-
    infer([
        if(>(float,float), [iplus(int,int)], [iminus(int,int)])
        ], Ret),
        assertion(Ret==int).

test(passing_variables, [nondet]) :-
    infer([
        (gvLet(var1, T, 5.6)), (gvLet(var2, T2, var1)), (gvLet(var3, T3, var1*var2))
        ], Ret),
        assertion(Ret==unit), assertion(T==T2).



:-end_tests(typeInf).

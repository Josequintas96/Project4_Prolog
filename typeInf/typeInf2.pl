:- dynamic gvar/2.
:- dynamic gdef/2.

typeExp(X, int) :-
    integer(X).

typeExp(X, float) :-
    float(X).

typeExp(X, bool) :-
    typeBoolExp(X).

/*
New addition
*/
typeExp(X, atom) :-
    atom(X).
/*
Jose addition
*/
typeExp((X, [A1, A2]),atom) :-
    typeExp(X, atom),
    typeExp(A1, atom),
    typeExp(A2, atom).

/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    typeExpList(Args, ArgsT),
    append(ArgsT, [T], FType), /* make it loook like a function signature */
    functionType(Fname, FType). /* get type of arguments from definition */



/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */


/*New addition */

argument(L):- 
    is_list(L),
    length(L,X),
    maplist(atomic, L).


hasComparison(int).
hasComparison(float).
hasComparison(string).
hasComparison(bool).

hasAdd(int).
hasAdd(float).

hasfloat(float).


%typeBoolExp( X & Y) :- 
%    typeExp(X, T),
%    typeExp(Y, T),
%    hasComparison(T).
%typeBoolExp( X | Y) :- 
 %   typeExp(X, T),
 %   typeExp(Y, T),
 %   hasComparison(T).        


/* TODO: add statements types and their type checking */

typeStatement(X, T) :-
    typeExp(X, T).

/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet((Name, Code),T), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */


% b. global function definitions (let add x y = x+y)
% last expression is an implicit return. return statement also possible  
% gvlet posses a name, argumentts (probably same type), a function itself (which probably has oen arguments)
    %   where name is an atom
    % 

    % typeStatement(def((beta, [Ar1, Ar2]), T, X+Z), unit).

typeStatement(def(((Name,List), Code) ,T), unit):- %code is X+Y
    atom(Name),
    argument(List), /* make sure we have a list */
    typeExp(Code, T),
    bType(T), /* make sure we have an infered type */
    asserta(gdef(Name, T)). /* add definition to database */



/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).

/* for statements are encodes as:
    for(variable: int, condition:Boolean, expression:variable:int -> variable:int, [Statments])
    https://discretemathisfun.wordpress.com/2009/12/07/looping-using-prolog/
*/
% typeStatement(for(Name, Type, V, Cond, Exp, Statements ), T) :-
%     typeStatement(gvLet(Name, Type, Code)),
%     typeBoolExp(Cond),
%     typeCode(Exp),
%     typeCode(Statemetns, T).


/* Switch statements are encodes as:
    Switch(choice:[case])
    OR
    Switch(choice, case1, case2, case 3)

    Choice is option looking for
    case=> two parts, 
*/
% typeStatement(switch(Choice, X), T) :-
%     hasComparison(Choice),
%     integer(X).




/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).


/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false). 
typeBoolExp( X < Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X > Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X >= Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X == Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

/*

iplus :: int -> int -> int

*/
fType((+), [T, T, T]) :- hasAdd(T).



fType((-), [T, T, T]) :- hasAdd(T).
fType((*), [T, T, T]) :- hasAdd(T).
fType((\/), [T, T, T]) :- hasAdd(T).

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).

fType(iminus, [int,int,int]).
fType(fminus, [float, float, float]).
fType(fmultiply, [float,float,float]).
fType(fdivide, [float,float,float]).


fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */ %????


fType(cos, [T,T]) :- hasfloat(T).
% ftype cos has a list T where T is float
fType(sin, [T,T]) :- hasfloat(T).
fType(sqrt, [T,T]) :- hasfloat(T).


/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().

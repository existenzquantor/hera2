:- module(hera_logic, [sat/2, consistent/1]).

consistent(M) :-
    findall(X, (member(X, M), negate(X, Y), member(Y, M)), L1),
    length(L1, 0),
    findall(X, (member(caused(X), M), negate(X, Y), member(Y, M)), L2),
    length(L2, 0).

nn(not(not(X)), N) :- 
    nn(X, N), !.
nn(X, X).
negate(X, N2) :-
    nn(X, N1),
    nn(not(N1), N2).
negate_all(L, N) :-
    negate_all(L, [], N).
negate_all([], E, E).
negate_all([A | T], L, E) :-
    negate(A, A2),
    negate_all(T, [A2 | L], E).

model(Models) :- 
    findall(X, assumable(X), L),
    mysubset(L, M),
    get_nots(M, L, MO),
    union(M, MO, Models).
get_nots(M, L, M2) :-
    findall(X, (assumable(X), \+member(X, M), member(X, L)), M1),
    negate_all(M1, M2).

mysubset([], []).
mysubset([_ | L1], L2) :-
    mysubset(L1, L2).
mysubset([E | L1], [E | L2]) :-
    mysubset(L1, L2).

sat(and(A, B), Model) :-
    model(Model),
    sat(A, Model),
    sat(B, Model).

sat(or(A, B), Model) :-
    model(Model),
    sat(A, Model);
    sat(B, Model).

sat(impl(A, B), Model) :-
    sat(or(not(A), B), Model).

sat(A, Model) :-
    model(Model),
    member(A, Model).

sat(not(A), Model) :-
    model(Model),
    \+ sat(A, Model).

sat(not(not(A)), Model) :-
    model(Model),
    sat(A, Model).
    
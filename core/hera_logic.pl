:- module(hera_logic, [sat/2, consistent/1]).

consistent(M) :-
    findall(X, (member(X, M), member(not(X), M)), L),
    length(L, 0).

model(Models) :- 
    findall(X, assumable(X), L),
    mysubset(L, M),
    get_nots(M, L, MO),
    union(M, MO, Models).
get_nots(M, L, MO) :-
    findall(not(X), (assumable(X), \+member(X, M), member(X, L)), MO).

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

sat(A, Model) :-
    model(Model),
    member(A, Model).

sat(not(A), Model) :-
    model(Model),
    \+ sat(A, Model).

sat(not(not(A)), Model) :-
    model(Model),
    sat(A, Model).
    
:- module(hera_logic, [sat/2, consistent/1, model/1, make_conjunction/2, nnf/2]).

consistent(M) :-
    findall(X, (member(X, M), negate(X, Y), member(Y, M)), L1),
    length(L1, 0),
    findall(X, (member(caused(X), M), negate(X, Y), member(Y, M)), L2),
    length(L2, 0).

nnf(not(not(F)), E) :-
    nnf(F, E), !.
nnf(not(and(F, G)), or(F2, G2)) :-
    nnf(not(F), F2),
    nnf(not(G), G2), !.
nnf(not(or(F, G)), and(F2, G2)) :-
    nnf(not(F), F2),
    nnf(not(G), G2), !.
nnf(not(impl(F, G)), and(F2, G2)) :-
    nnf(F, F2),
    nnf(not(G), G2), !.
nnf(impl(F, G), or(F2, G2)) :-
    nnf(not(F), F2),
    nnf(G, G2), !.
nnf(not(biimpl(F, G)), not(and(F2, G2))) :-
    nnf(impl(F, G), F2),
    nnf(impl(G, F), G2), !.
nnf(biimpl(F, G), and(F2, G2)) :-
    nnf(impl(F, G), F2),
    nnf(impl(G, F), G2), !.
nnf(and(F, G), and(F1, G1)) :-
    nnf(F, F1), 
    nnf(G, G1), !.
nnf(or(F, G), or(F1, G1)) :-
    nnf(F, F1), 
    nnf(G, G1), !.
nnf(F, F).

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
    findall(X, holds(X), M),
    findall(X, (holds(Y), negate(Y, X)), M2),
    union(M, M2, M3),
    mysubset(M3, Models),
    length(M, N),
    length(Models, N),
    consistent(Models).

mysubset([], []).
mysubset([_ | L1], L2) :-
    mysubset(L1, L2).
mysubset([E | L1], [E | L2]) :-
    mysubset(L1, L2).

sat(A, Model) :-
    member(A, Model), !.
sat(and(A, B), Model) :-
    sat(A, Model),
    sat(B, Model), !.
sat(or(A, _), Model) :-
    sat(A, Model), !.
sat(or(_, B), Model) :-
    sat(B, Model), !.

make_conjunction(L, F) :-
    make_conjunction(L, [], F).
make_conjunction([], L, L).
make_conjunction([A | R], [], E) :-
    make_conjunction(R, A, E), !.
make_conjunction([A | R], L, E) :-
    make_conjunction(R, and(A, L), E).
:- module(hera_logic, [sat/1, sat/2, consistent/1, model/1, make_conjunction/2, nnf/2, reachable_state/1, negate/2]).
:- use_module("../../causality/core/interpreter.pl", [generate_plan/3]).


%! consistent(+M)
% True if M is a consistent set of literals
consistent(M) :-
    findall(X, (member(X, M), negate(X, Y), member(Y, M)), L1),
    length(L1, 0),
    findall(X, (member(caused(X), M), negate(X, Y), member(Y, M)), L2),
    length(L2, 0),
    findall(X, (member(caused(X), M), negate(X, Y), member(caused(Y), M)), L3),
    length(L3, 0),
    findall(X, (member(X, M), X = not(geq(A, A))), L4),
    length(L4, 0),
    findall(X, (member(X, M), member(Y, M), member(Z, M), X = geq(C, B), Y = geq(B, A), Z = not(geq(C, A))), L5),
    length(L5, 0).

%! nnf(+F, -E)
% True if E is the negation normal form of F
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

%! reachable_state(+S)
%  True if there is a plan that transitions the initial state to S
reachable_state(S) :-
    init(M),
    length(M, N),
    negate_all(M, MNeg),
    union(M, MNeg, MU),
    mysubset(MU, S),
    length(S, N),
    consistent(S),
    Len is N**2,
    generate_plan(Len, S, _).

%! model(+Model)
% True if Model contains L or not(L) for each literal L in the domain
model(Model) :- 
    findall(X, holds(X), M),
    length(M, N),
    findall(X, (holds(Y), negate(Y, X)), M2),
    union(M, M2, M3),
    mysubset(M3, Model),
    length(Model, N),
    consistent(Model).

mysubset([], []).
mysubset([_ | L1], L2) :-
    mysubset(L1, L2).
mysubset([E | L1], [E | L2]) :-
    mysubset(L1, L2).

%! sat(+Formula, +Model)
% True if the Model satisfies the Formula
sat(A, Model) :-
    member(A, Model), !.
sat(and(A, B), Model) :-
    sat(A, Model),
    sat(B, Model), !.
sat(or(A, _), Model) :-
    sat(A, Model), !.
sat(or(_, B), Model) :-
    sat(B, Model), !.
sat(geq(A, B)) :-
    sum_util(A, SA),
    sum_util(B, SB),
    SA >= SB.

is_literal(L) :-
    atom(L).
is_literal(not(L)) :-
    atom(L).

sum_util(and(A, B), U) :-
    sum_util(A, UA), !,
    sum_util(B, UB), !,
    U is UA + UB.
sum_util(C, U) :-
    utility(C, U), !.
sum_util(_, 0).



%! make_conjunction(+List, -Formula)
% True if Formula is the conjunction of the formulas in List
make_conjunction(L, F) :-
    make_conjunction(L, [], F).
make_conjunction([], L, L).
make_conjunction([A | R], [], E) :-
    make_conjunction(R, A, E), !.
make_conjunction([A | R], L, E) :-
    make_conjunction(R, and(A, L), E).

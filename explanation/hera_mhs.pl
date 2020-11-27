:- module(hera_mhs, [mhs/2]).
:- use_module(hera_logic, [consistent/1]).

% Minimal Hitting Sets M of the list of lists L
mhs(M, L) :-
    findall(X, (hs(X, L), consistent(X)), H),
    filterMin(H, H, [], M).

/*
* hs(+HittingSet, +SetOfSets)
*/
hs(H, L) :-
   length(L, I),
    hs([], L, 0, I, H).
hs(S, _, N, N, S).
hs(H, L, M, N, S) :-
    M < N,
    nth0(M, L, Sub),
    intersection(Sub, H, I),
    length(I, IN),
    IN == 0,
    member(X, Sub),
    M2 is M + 1,
    append(H, [X], H2),
    hs(H2, L, M2, N, S).
hs(H, L, M, N, S) :-
    M < N,
    nth0(M, L, Sub),
    intersection(Sub, H, I),
    length(I, IN),
    IN > 0,
    M2 is M + 1,
    hs(H, L, M2, N, S).

hasSubset(H, [I | _]) :-
    H \= I,
    subset(I, H), !.
hasSubset(H, [_ | L]) :-
    hasSubset(H, L).

filterMin(_, [], L, L).
filterMin(H, [A | R1], L, Erg) :-
    \+ hasSubset(A, H),
    filterMin(H, R1, [A | L], Erg).
filterMin(H, [A | R1], L, Erg) :-
    hasSubset(A, H),
    filterMin(H, R1, L, Erg).
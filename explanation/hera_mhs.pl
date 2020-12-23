:- module(hera_mhs, [mhs/2, filterduplicates/2]).
:- use_module(hera_logic, [consistent/1]).

%! mhs(-M, +L)
% True if M is a hitting sets of the list of lists L
mhs(M, L) :-
    findall(X, (hs(X, L), consistent(X)), HC),
    filterMin(HC, HC, [], M).

filterConsistent(H, HC) :-
    filterConsistent(H, [], HC).
filterConsistent([], HC, HC).
filterConsistent([L | R], H, HC) :-
    consistent(L),
    filterConsistent(R, [L | H], HC).
filterConsistent([L | R], H, HC) :-
    \+consistent(L),
    filterConsistent(R, H, HC).

filterduplicates(L, L2) :-
    filterduplicates(L, [], L2).
filterduplicates([], L, L).
filterduplicates([X | R], L, E) :-
    \+ hasEqual(X, L),
    filterduplicates(R, [X | L], E).
filterduplicates([X | R], L, E) :-
    hasEqual(X, L),
    filterduplicates(R, L, E).

%! hs(-HittingSet, +ListOfLists)
% True if HittingSet hits every list in ListOfLists
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

hasEqual(H, [I | J]) :-
    ((subset(I, H),
      subset(H, I)) -> true; hasEqual(H, J)).

hasSubset(H, [I | J]) :-
    ((H \= I, H \= [],
    subset(I, H), \+subset(H, I)) -> true; hasSubset(H, J)).

filterMin(_, [], L, L).
filterMin(H, [A | R1], L, Erg) :-
    \+ hasSubset(A, H),
    filterMin(H, R1, [A | L], Erg).
filterMin(H, [A | R1], L, Erg) :-
    hasSubset(A, H),
    filterMin(H, R1, L, Erg).
:- module(hera_explain, [necc_reasons/2, suff_reasons/2, inus_reasons/2]).
:- use_module(hera_logic, [sat/2]).
:- use_module(hera_mhs, [mhs/2]).

necc_reasons(F, R) :-
    findall(M, sat(F, M), L),
    mhs(H, L),
    findall(Holds, holds(Holds), HoldsList),
    findall(X, (member(Y, H), intersection(Y, HoldsList, X)), R).

suff_reasons(F, H) :-
    necc_reasons(F, I),
    mhs(H, I).

inus_reason(F, R) :-
    suff_reasons(F, S),
    necc_reasons(F, N),
    member(R, N),
    is_part_of_suff(R, S).

inus_reasons(F, H) :-
    findall(R, inus_reason(F, R), H).

is_part_of_suff(R, [A | _]) :-
    subset(R, A), !.
is_part_of_suff(R, [_ | L]) :-
    is_part_of_suff(R, L).
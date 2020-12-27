:- module(hera_explain, [reasons/2]).
:- use_module(hera_logic, [sat/2, model/1]).
:- use_module(hera_mhs, [mhs/2, filterduplicates/2]).

reasons(F, {"suff":S,"necc":N,"inus":R}) :-
    necc_reasons(F, N),
    suff_reasons(N, S),
    inus_reasons(S, N, R).
    
necc_reasons(F, R) :-
    findall(M, (model(M), sat(F, M)), L), 
    filterduplicates(L, L2),
    mhs(H, L2),
    findall(Holds, holds(Holds), HoldsList),
    findall(X, (member(Y, H), intersection(Y, HoldsList, X)), K),
    filterduplicates(K, R).

suff_reasons(Necc, H) :-
    mhs(H, Necc).

inus_reason(Suff, Necc, R) :-
    member(R, Necc),
    is_part_of_suff(R, Suff).

inus_reasons(Suff, Necc, H) :-
    findall(R, inus_reason(Suff, Necc, R), H).

is_part_of_suff(R, [A | _]) :-
    subset(R, A), !.
is_part_of_suff(R, [_ | L]) :-
    is_part_of_suff(R, L).
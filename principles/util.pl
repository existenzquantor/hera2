:- module(principle, [  holds/1,
                        principle_formula/1,
                        is_permissible/0,
                        explain_judgment/1]).
:- dynamic  holds/1,
            principle_formula/1.
:- use_module("../explanation/hera_logic.pl", [sat/1, sat/2,make_conjunction/2, nnf/2, reachable_state/1]).
:- use_module("../../causality/core/interpreter.pl", [finally/2]).
:- use_module("./explanation/hera_explain.pl", [reasons/2]).

init_principle :-
    setof(X, reachable_state(X), S),
    assertz(reachable_states(S)),
    plan(P),
    findall(X, finally(P, X), I),
    make_conjunction(I, IC),
    findall(geq(IC, X), (\+((subset(IC, X), subset(X, IC))), member(M, S), make_conjunction(M, X)), L_util),
    make_conjunction(L_util, F),
    assertz(principle_formula(F)),
    prepare_model.

prepare_model :-
    reachable_states(S),
    plan(P),
    findall(X, finally(P, X), I),
    make_conjunction(I, IC),
    forall((member(X, S), make_conjunction(X, XC), sat(geq(IC, XC))), assertz(holds(geq(IC, XC)))),
    forall((member(X, S), make_conjunction(X, XC), \+ holds(geq(IC, XC))), assertz(holds(not(geq(IC, XC))))).

is_permissible :-
    findall(X, holds(X), Model),
    principle_formula(F),
    sat(F, Model).

explain_judgment(D) :-
    principle_formula(F),
    (is_permissible -> 
        reasons(F, D);
         (nnf(not(F), F2), reasons(F2, D))
    ).

:- init_principle.
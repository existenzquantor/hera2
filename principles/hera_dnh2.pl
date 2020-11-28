:- module(principle, [  holds/1,
                        principle_formula/1,
                        is_permissible/0,
                        explain_judgment/1]).
:- dynamic  holds/1,
            principle_formula/1.
:- use_module("../explanation/hera_logic.pl", [sat/2,make_conjunction/2, nnf/2]).
:- use_module("../../causality/core/causality.pl", [cause_empty_temporal/3]).
:- use_module("./explanation/hera_explain.pl", [reasons/2]).

init_principle :-
    findall(impl(caused(X), not(bad(X))), utility(X, _), L),
    make_conjunction(L, C),
    nnf(C, F),
    assertz(principle_formula(F)),
    prepare_model.

prepare_model :-
    plan(Program), 
    forall((utility(X, _), cause_empty_temporal(Program, X, _)), assertz(holds(caused(X)))),
    forall((utility(X, _), \+cause_empty_temporal(Program, X, _)), assertz(holds(not(caused(X))))),
    forall((utility(X, N), N < 0), assertz(holds(bad(X)))),
    forall((utility(X, N), N >= 0), assertz(holds(not(bad(X))))).

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
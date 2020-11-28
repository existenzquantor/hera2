:- module(principle, [  holds/1,
                        principle_formula/1,
                        is_permissible/0,
                        explain_judgment/1]).
:- dynamic  holds/1,
            principle_formula/1.
:- use_module("../explanation/hera_logic.pl", [sat/2,make_conjunction/2, nnf/2]).
:- use_module("../../causality/core/causality.pl", [cause_empty_temporal/3]).
:- use_module("../../causality/core/interpreter.pl", [finally/2]).
:- use_module("./explanation/hera_explain.pl", [reasons/2]).

init_principle :-
    findall(impl(means(X), end(X)), affects(_, X, _), L),
    list_to_set(L, S),
    make_conjunction(S, C),
    nnf(C, F),
    assertz(principle_formula(F)),
    prepare_model.

prepare_model :-
    plan(Program), 
    forall((finally(Program, X), cause_empty_temporal(Program, X, _), affects(X, P, _)), assertz(holds(means(P)))),
    forall((goal(X), member(F, X), affects(F, P, pos)), assertz(holds(end(P)))),
    forall((affects(_, P, _), \+ holds(end(P))), assertz(holds(not(end(P))))),
    forall((affects(_, P, _), \+ holds(means(P))), assertz(holds(not(means(P))))).

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
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
:- use_module("../../causality/core/programs.pl", [program_to_list/2]).
:- use_module("../../causality/core/instrumentality.pl", [instrumental/3]).

init_principle :-
    plan(Program), 
    program_to_list(Program, PL),
    goal(G),
    findall(not(bad(X)), (member(X, PL), \+number(X)), L_deon),
    findall(impl(goal(X), not(bad(X))), member(X, G), L_goodgoals),
    findall(impl(instrumental(X), not(bad(X))), finally(Program, X), L_dnih),
    findall(X, finally(Program, X), S1),
    findall(X, finally(empty, X), S2),
    make_conjunction(S1, C1),
    make_conjunction(S2, C2),
    append(L_dnih, L_goodgoals, L_temp),
    append(L_temp, L_deon, L),
    list_to_set([geq(C1, C2) | L], S),
    make_conjunction(S, C),
    nnf(C, F),
    assertz(principle_formula(F)),
    prepare_model.

prepare_model :-
    deon_constraint,
    good_goal_constraint,
    dnih_constraint,
    proportionality.

proportionality :-
    plan(Program),
    findall(X, finally(Program, X), S1),
    findall(X, finally(empty, X), S2),
    make_conjunction(S1, C1),
    make_conjunction(S2, C2),
    findall(X, holds(X), Model),
    (sat(geq(C1, C2), Model) -> assertz(holds(geq(C1, C2)))
    ; assertz(holds(not(geq(C1, C2))))
    ).
    

good_goal_constraint :-
    goal(G), 
    forall(member(X, G), assertz(holds(goal(X)))).

dnih_constraint :-
    plan(Program), 
    forall((utility(X, _), finally(Program, X), cause_empty_temporal(Program, X, _), goal(G), member(GF, G), instrumental(Program, X, GF)), assertz(holds(instrumental(X)))),
    forall((utility(X, _), finally(Program, X), \+holds(instrumental(X))), assertz(holds(not(instrumental(X))))),
    forall((utility(X, N), N < 0, finally(Program, X)), assertz(holds(bad(X)))),
    forall((utility(X, N), N >= 0, finally(Program, X)), assertz(holds(not(bad(X))))).

deon_constraint :-
    plan(Program), 
    program_to_list(Program, L),
    forall((member(A, L), utility(A, U), U < 0), assertz(holds(bad(A)))),
    forall((member(A, L), \+holds(bad(A))), assertz(holds(not(bad(A))))).

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
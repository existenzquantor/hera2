:- module(principle, [  holds/1,
                        principle_formula/1,
                        is_permissible/0,
                        explain_judgment/1]).
:- dynamic  holds/1,
            principle_formula/1.
:- use_module("../explanation/hera_logic.pl", [sat/2,make_conjunction/2, nnf/2]).
:- use_module("../../causality/core/programs.pl", [program_to_list/2]).
:- use_module("./explanation/hera_explain.pl", [reasons/2]).

init_principle :-
    plan(Program), 
    program_to_list(Program, PL),
    findall(not(bad(X)), (member(X, PL), \+number(X)), L),
    list_to_set(L, S),
    make_conjunction(S, C),
    nnf(C, F),
    assertz(principle_formula(F)),writeln(F),
    prepare_model.

prepare_model :-
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
:- use_module("../explanation/hera_logic.pl", [make_conjunction/2]).

principle_formula(F) :-
    findall(impl(caused(X), not(bad(X))), utility(X, _), L),
    make_conjunction(L, F).


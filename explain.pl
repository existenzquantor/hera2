:- use_module("./explanation/hera_explain.pl", [reasons/2]).
:- compile("./domains/trolley.pl").
:- compile("./principles/hera_dnh.pl").

% TODO: World has to be computed rather than asserted
holds(not(caused(dead1))).
holds(bad(dead1)).
holds(not(bad(not(dead1)))).
holds(not(caused(dead5))).
holds(bad(dead5)).
holds(not(bad(not(dead5)))).
holds(not(caused(not(dead1)))).
holds(not(caused(not(dead5)))).

:- principle_formula(F), reasons(F, D), writeln(D).
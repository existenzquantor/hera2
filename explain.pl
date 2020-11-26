:- use_module("./core/hera_explain.pl", [suff_reasons/2, necc_reasons/2, inus_reasons/2]).

assumable(a).
assumable(b).

holds(not(a)).

:- suff_reasons(or(not(a), b), S), write("Suff: " ), writeln(S).
:- necc_reasons(or(not(a), b), N), write("Necc: " ), writeln(N).
:- inus_reasons(or(not(a), b), I), write("Inus: " ), writeln(I).
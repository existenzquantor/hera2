:- use_module("./core/hera_explain.pl", [suff_reasons/2, necc_reasons/2, inus_reasons/2]).

assumable(caused(a)).
assumable(bad(a)).
%assumable(a).
%assumable(b).

holds(not(caused(a))).
holds(bad(a)).
%holds(a).
%holds(not(b)).

formula(impl(caused(a), not(bad(a)))).
%formula(impl(a, not(b))).

:- formula(F), suff_reasons(F, S), write("Suff: " ), writeln(S).
:- formula(F), necc_reasons(F, N), write("Necc: " ), writeln(N).
:- formula(F), inus_reasons(F, I), write("Inus: " ), writeln(I).
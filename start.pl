:- compile("./domains/trolley_dnh_permissible.pl").
:- compile("./domains/trolley_dnh_impermissible.pl").
:- compile("./principles/hera_dnh.pl").

:- init_principle, writeln("Using Do No Harm Principle.").
:- prepare_model, writeln("Situation Model Compiled.").
:- is_permissible -> writeln("permissible"); writeln("impermissible").
:- writeln("Explanation started (this may take some time...)."), explain_judgment.
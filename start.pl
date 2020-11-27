:- compile("./domains/trolley_dnh_permissible.pl").
%:- compile("./domains/trolley_dnh_impermissible.pl").
:- use_module("./principles/hera_dnh.pl").
%:- use_module("./principles/hera_dnh2.pl").

start :-    init_principle, writeln("Using Do No Harm Principle."),
            principle_formula(F), writeln(F),
            prepare_model, writeln("Situation Model Compiled."),
            (is_permissible -> writeln("permissible"); writeln("impermissible")),
            writeln("Explanation started (this may take some time...)."), 
            explain_judgment(Ex),
            writeln(Ex).

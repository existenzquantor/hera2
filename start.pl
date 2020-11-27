:- compile("./domains/flowers.pl").
%:- compile("./domains/trolley_dnh_permissible.pl").
%:- compile("./domains/trolley_dnh_impermissible.pl").
:- use_module("./principles/hera_ci.pl").
%:- use_module("./principles/hera_dnh.pl").
%:- use_module("./principles/hera_dnh2.pl").

start :-    (is_permissible -> writeln("permissible"); writeln("impermissible")),
            writeln("Explanation started (this may take some time...)."), 
            explain_judgment(Ex),
            writeln(Ex).

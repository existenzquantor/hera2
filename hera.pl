#!/usr/bin/env swipl
:- initialization(main, main).

main(Argv) :-
        nth0(0, Argv, Domain),
        nth0(1, Argv, Principle),
        string_concat("./domains/", Domain, StrDomain0),
        string_concat(StrDomain0, ".pl", StrDomain),
        compile(StrDomain),
        string_concat("./principles/hera_", Principle, StrPr0),
        string_concat(StrPr0, ".pl", StrPr),
        use_module(StrPr),
        nth0(2, Argv, Task),
        (Task == evaluate -> 
            (is_permissible -> writeln("true"); writeln("false"));
            (Task == explain -> 
                (explain_judgment(Ex), writeln(Ex)); true
            )
        ).

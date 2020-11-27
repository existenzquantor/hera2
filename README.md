Ongoing work towards a Prolog reimplementation of HERA. Depends on the causality library found here: https://github.com/existenzquantor/causality .


```prolog
effect(give_flowers, [], [celia_happy]).
effect(wait, [], []).
effect(0, [celia_happy], [alice_happy]).

init([not(celia_happy), not(alice_happy)]).
goal([alice_happy]).

plan(give_flowers).

affects(celia_happy, celia, pos).
affects(alice_happy, alice, pos).
```
```bash
> ./hera.pl flowers ci evaluate                                                                                        [16:36:44]
false
```

```bash
> /hera.pl flowers humanity explain 
{suff:[[means(celia),not(goal(celia))]],necc:[[means(celia)],[not(goal(celia))]],inus:[[means(celia)],[not(goal(celia))]]}
```

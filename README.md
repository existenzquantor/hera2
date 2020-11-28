Ongoing work towards a Prolog reimplementation of HERA. Currently, it is assumed that the folder containing the causality library (https://github.com/existenzquantor/causality) is located next to the hera2 folder, i.e., in some common parent folder.


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

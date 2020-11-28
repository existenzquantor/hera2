Ongoing work towards a Prolog reimplementation of HERA. Currently, it is assumed that the folder containing the causality library (https://github.com/existenzquantor/causality) is located next to the hera2 folder, i.e., in some common parent folder.

The input to hera consists of a formal description of the domain, task, plan, and ethical properties of actions and facts. The following description models the famous Giving-Flowers Example (cf., https://github.com/existenzquantor/ethics).

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

The first line represents the action give_flowers. The action has no preconditions (this explains why the second argument is an empty list) and the effect of that action is that celia will be happy (as expressed by the third argument). The second line represents the empty action (doing nothing). The third line models an event rather than an action. This is signalled by the first argument being an integer rather than a name. By convention, numbers start counting with 0. Thus, the expression says that after action 0 (viz., the first action in an action sequence), an event will fire that will make alice happy (effect) if celia is already happy (precondition). The next line represents the initial situation, viz., neither celia nor alice are happy. The goal is to make alice happy. The plan is to give flowers. This plan indeed achieves the goal: Give flowers will make celia happy, then the event will fire and alice will be happy as well. (Note that all events' preconditions are evaluated at the state that results from the performed action. This way, infinite fire chains of events are not possible.) The last two lines represent knowledge about how facts affect persons: The truth of celia_happy affects celia positively and alice_happy affects alice positively. (The first argument can also be a negated fact, and the third argument can be neg instead of pos.)

To see if giving flowers to celia is morally permissible, you can run:
```bash
> ./hera.pl flowers humanity evaluate                                                                                        [16:36:44]
false
```
For this to work, SWI Prolog must be installed (so, the swipl command should invoke the prolog environment). The first argument denotes the name of the domain description. The program assumes that the domain description will be found at the location ./domains/<name>.pl. The second argument denotes the moral principle to be used. In this case we use the Kantian Humanity Principle (cf., https://plato.stanford.edu/entries/persons-means/). The third argument specifies the task. In this case, the plan should be evaluated. As an answer you can expect either true (the plan is morally permissible) or false (the plan is morally impermissible).


```bash
> /hera.pl flowers humanity explain 
{suff:[[means(celia),not(goal(celia))]],necc:[[means(celia)],[not(goal(celia))]],inus:[[means(celia)],[not(goal(celia))]]}
```

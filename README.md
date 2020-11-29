# Brief Description
This repository contains work towards a Prolog reimplementation of HERA (see https://github.com/existenzquantor/ethics). 

# Installation
SWI Prolog (https://www.swi-prolog.org/) must be installed on the system to run the HERA program. SWI Prolog is available for many operating systems. In a MacOS environment, you can install SWI Prolog using brew by typing ```brew install swipl```, under Ubuntu Linux it is ```apt install swipl```, and binaries for Windows are also available on the SWI Prolog download website. 

To get started, first clone this github repository. Currently, it is assumed that the folder containing the causality library (https://github.com/existenzquantor/causality) is located next to the hera2 folder, i.e., in some common parent folder. Thus, you may need to clone the causality github repository, as well.

# Using hera2
The input to hera2 consists of a formal description of the domain, task, plan, and ethical properties of actions and facts. The following description models the famous Giving-Flowers Example (cf., https://github.com/existenzquantor/ethics).

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

To see if giving flowers to celia is morally permissible, you can run the hera.pl script located in the hera2 folder:
```bash
> ./hera.pl flowers humanity evaluate                                                                                        [16:36:44]
false
```
For this to work, SWI Prolog must be installed (so, the swipl command should invoke the prolog environment), and the hera.pl script must be set as executable. Under unix-like systems, this can be achieved by typing ```chmod +x hera.pl```. 

The first argument in the command ```./hera.pl flowers humanity evaluate``` denotes the name of the domain description. The program assumes that the domain description will be found at the location ```./domains/<name>.pl```. The second argument denotes the moral principle to be used. In this case we use the Kantian Humanity Principle (cf., https://plato.stanford.edu/entries/persons-means/). See below for a list of available principles. The third argument specifies the task. In this case, the plan should be evaluated. As an answer you can expect either true (the plan is morally permissible) or false (the plan is morally impermissible). The flower example is impermissible. This is because celia is used merely as a means to make alice happy. This can be fixed by adding celia_happy to the goal---now celia is also used as an end.
For this to work, SWI Prolog must be installed (so, the swipl command should invoke the prolog environment), and the hera.pl script must be set as executable. Under unix-like systems, this can be achieved by typing ```chmod +x hera.pl```. The first argument denotes the name of the domain description. The program assumes that the domain description will be found at the location ./domains/<name>.pl. The second argument denotes the moral principle to be used. In this case we use the Kantian Humanity Principle (cf., https://plato.stanford.edu/entries/persons-means/). The third argument specifies the task. In this case, the plan should be evaluated. As an answer you can expect either true (the plan is morally permissible) or false (the plan is morally impermissible). The flower example is impermissible. This is because celia is used merely as a means to make alice happy. This can be fixed by adding celia_happy to the goal---now celia is also used as an end.

HERA also comes with an inbuilt explanation functionality. This can be invoked by specifying the task "explain":
```bash
> ./hera.pl flowers humanity explain 
{suff:[[means(celia),not(end(celia))]],necc:[[means(celia)],[not(end(celia))]],inus:[[means(celia)],[not(end(celia))]]}
```

The explanation nicely fits to the explanation I have given above: The reason why giving flowers is impermissible is that celia is a means but not an end (suff = sufficient reason). Moreover, the output refers to necessary reasons (necc): Using celia as a means was necessary for the plan being impermissible, and so was not using celia as an end. From this one can extract two repair strategies: Either find a plan that does not use celia as a means, or use celia as an end. The last entry in the resulting dictionary refers to INUS reasons. INUS reasons are necessary parts of sufficient reasons. They do not always exist, but if they exist they tend to be shorter and more to the point as compared to suff and necc reasons. In this toy example, necc and inus are equal.

# Available Principles

* ```deon```: Simple Deontology Principle: Plan is permissible iff the plan does not contain any morally bad actions.
* ```dnh```: Do No Harm Principle: Plan is permissible iff none of the harm in the final state is caused by the actions in the plan.
* ```dnih```: Do No Instrumental Harm Principle: Plan is permissible iff none of the harm in the final state is instrumental for achieving the goal, i.e., harm is only allowed as side effect.
* ```humanity```: Kantian Humanity Principle: Plan is permissible iff everybody affected by some caused fact is positively affected by a goal.
* ... to be continued ...

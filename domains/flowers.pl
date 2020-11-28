init([not(celia_happy), not(alice_happy)]).

effect(give_flowers, [], [celia_happy]).
effect(wait, [], []).
effect(0, [celia_happy], [alice_happy]).

goal([alice_happy]).

plan(give_flowers).

affects(celia_happy, celia, pos).
affects(alice_happy, alice, pos).

utility(celia_happy, 1).
utility(alice_happy, 1).
:- dynamic holds/1.

init([not(dead1), not(dead5), not(left)]).
effect(pull, [], [left]).
effect(wait, [], []).
effect(0, [left], dead1).
effect(0, [not(left)], dead5).

plan(pull).

utility(dead1, -1).
utility(not(dead1), 1).
utility(dead5, -5).
utility(not(dead5), 5).
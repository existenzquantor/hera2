nit([not(man_on_track), not(dead)]).

effect(push, [], [man_on_track]).
effect(0, [not(man_on_track)], [dead]).

goal([not(dead)]).
plan(push).

utility(man_on_track, -1).
utility(dead, -1).
utility(push, -1).
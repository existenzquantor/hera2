effect(push, [], [man_on_track]).
effect(0, [not(man_on_track)], [dead]).

init([not(man_on_track), not(dead)]).
plan(push).
goal([not(dead)]).


utility(man_on_track, -1).
utility(dead, -1).
utility(push, -1).
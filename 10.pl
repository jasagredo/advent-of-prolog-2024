:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

/*
Only 'score/7' changes between parts.

Part 1:
  Move to every direction that contains Cur+1. Accumulate the ends in an assoc
  to remove duplicates.

Part 2:
  Move to every direction that contains Cur+1. Whenever we find a 9, sum 1 to
  the accumulated result.
*/

matrix(_, _, _, A, A) --> call(eos).
matrix(C, _, Y, A0, A1) -->
    "\n",
    { Y1 #= Y + 1 },
    matrix(C, 0, Y1, A0, A1).
matrix(C, X, Y, A0-Xs, Res) -->
    [Char],
    { dif(Char, '\n'), number_chars(N, [Char]), put_assoc(X-Y, A0, N, A1), X1 #= X + 1 },
    { if_(=(Char,C), Xs1 = [X-Y|Xs], Xs1 = Xs) },
    matrix(C, X1, Y, A1-Xs1, Res).

score(P, Map, X-Y, Z0, Z1) :-
    score(P, X-Y, Map, X-Y, 0, Z0, Z1).
score(p1, Initial, _, Pos, 9, Z0, Z1) :-
    put_assoc(Initial-Pos, Z0, mt, Z1).
score(p2, _, _, _, 9, Z0, Z1) :-
    Z1 #= Z0 + 1.
score(P, Initial, Map, X-Y, Cur, Z0, Z1) :-
    Xp #= X + 1,
    Xm #= X - 1,
    Yp #= Y + 1,
    Ym #= Y - 1,
    Cur1 #= Cur + 1,
    filter(\Pos^get_assoc(Pos, Map, Cur1), [Xm-Y, Xp-Y, X-Ym, X-Yp], NewPositions),
    foldl(\Pos^score(P, Initial, Map, Pos, Cur1), NewPositions, Z0, Z1).

part1(A1, Zeroes, Solution) :-
    empty_assoc(A0),
    foldl(score(p1, A1), Zeroes, A0, Trails),
    assoc_to_keys(Trails, TrailsK),
    length(TrailsK, Solution).

part2(A1, Zeroes, Solution) :-
    foldl(score(p2, A1), Zeroes, 0, Solution).

real("10.txt").
sample("10.sample").

run :-
    time(
        (
            empty_assoc(A0),
            phrase_from_file(matrix('0', 0, 0, A0-[], A1-Zeroes), "10.txt"),
            part1(A1, Zeroes, X),
            format("Task 1: ~w~n", [X]),
            part2(A1, Zeroes, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

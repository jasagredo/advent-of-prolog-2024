:- use_module(dcgs/dcgs_utils).

/*
Part 1:
  Parse the "mul" blocks, multiply while parsing, then sum.

Part 2:
  Switch the parser on/off when finding a do or a don't.
*/

real("03.txt").
sample("03.sample").

mul(XY) --> "mul(", number(X), ",", number(Y), ")", { XY #= X * Y }.

input([X|Xs]) --> ..., mul(X), input(Xs).
input([])     --> ..., call(eos).

part1(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(input(Xs), F),
    sum_list(Xs, Sol).

input2(_, [])      --> call(eos).
input2(_, Xs)      --> "don't()", input2(dont, Xs).
input2(do, [X|Xs]) --> mul(X),    input2(do, Xs).
input2(_, Xs)      --> "do()",    input2(do, Xs).
input2(Do, Xs)     --> [_],       input2(Do, Xs).

part2(Mode, Sol) :-
    call(Mode, F),
    phrase_from_file(input2(do, Xs), F),
    sum_list(Xs, Sol).

run :-
    time(
        (
            part1(real, X),
            format("Task 1: ~w~n", [X]),
            part2(real, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

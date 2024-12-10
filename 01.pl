:- use_module(sort).
:- use_module(dcgs/dcgs_utils).
:- use_module(fp).

/*
Part 1:
  Mergesort both lists and then calculate the distances.

Part 2:
  1. Accumulate for each number how many times it appears on each list in pairs
     Number-Occurrences.
  2. Compute the similarity.
*/

input_format([], []) --> call(eos).
input_format([X|Xs], [Y|Ys]) -->
    number(X),
    "   ",
    number(Y),
    eol,
    input_format(Xs, Ys).

distance(X, Y, Z0, Z) :-
    Z #= Z0 + abs(X - Y).

part1(Xs, Ys, Solution):-
    mergesort(Xs, Xss),
    mergesort(Ys, Yss),
    foldl(distance, Xss, Yss, 0, Solution).

input_format2(Xs, Ys) -->
    call(eos),
    { empty_assoc(Xs), empty_assoc(Ys) }.
input_format2(Xss, Yss) -->
    number(X),
    "   ",
    number(Y),
    eol,
    input_format2(Xs, Ys),
    { get_assoc(X, Xs, N0), N1 #= N0 + 1, put_assoc(X, Xs, N1, Xss);
      put_assoc(X, Xs, 1, Xss)
    },
    { get_assoc(Y, Ys, N0), N1 #= N0 + 1, put_assoc(Y, Ys, N1, Yss);
      put_assoc(Y, Ys, 1, Yss)
    }.

similarity(Assoc, K-V, Z0, Z) :-
    get_assoc(K, Assoc, Reps),
    Z #= Z0 + K * V * Reps;
    Z #= Z0.

part2(Xs, Ys, Solution):-
    assoc_to_list(Xs, Xss),
    foldl(similarity(Ys), Xss, 0, Solution).

real("01.txt").
sample("01.sample").

run :-
    time(
        (
            F = "01.txt",
            phrase_from_file(input_format(Xs, Ys), F),
            part1(Xs, Ys, X),
            format("Task 1: ~w~n", [X]),
            phrase_from_file(input_format2(Xs2, Ys2), F),
            part2(Xs2, Ys2, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).

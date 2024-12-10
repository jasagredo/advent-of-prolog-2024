:- use_module(dcgs/dcgs_utils).
:- use_module(dcgs/dcgs_debug).
:- use_module(sort).
:- use_module(library(iso_ext)).

:- dynamic(ordered/2).

/*
Part 1:
  1. Parse the order and assert each relation in 'ordered/2'.
  2. Find which ones are wrongly sorted, i.e. for all pairs (sub-lists) [X,Y]
     where there are no ordered(Y, X).

Part 2:
  1. Ditto
  2. For the unsorted ones, mergesort them.
*/

real("05.txt").
sample("05.sample").

% parse

order, "\n" --> "\n".
order --> number(X), "|", number(Y), "\n", { assertz(ordered(X, Y)) }, order.

manual([X]) --> number(X), "\n".
manual([X|Xs]) --> number(X), ",", manual(Xs).

manuals([]) --> call(eos).
manuals([X|Xs]) --> manual(X), manuals(Xs).

input(X) --> order, "\n", manuals(X).

% logic

sublist([], []).
sublist([_|T0], T1) :-
    sublist(T0, T1).
sublist([H|T0], [H|T1]) :-
    sublist(T0, T1).

is_sorted(L) :-
    forall(sublist(L, [X,Y]), \+ ordered(Y, X)).

middle(L, Mid) :-
    length(L1, X),
    length(L2, X),
    append([L1, [Mid], L2], L).

% part 1

if_sorted(L, Acc0, Acc) :-
    is_sorted(L), middle(L,Mid), Acc #= Acc0 + Mid;
    Acc #= Acc0.

part1(X, Sol) :-
    foldl(if_sorted, X, 0, Sol).

% part 2

sort_then(L, Acc0, Acc) :-
    \+ is_sorted(L),
    mergesort_by(ordered, L, L1),
    middle(L1, Mid),
    Acc #= Acc0 + Mid;
    Acc #= Acc0.

part2(X, Sol) :-
    foldl(sort_then, X, 0, Sol).

run :-
    time(
        (
            phrase_from_file(input(Xs), "05.txt"),
            part1(Xs, X),
            format("Task 1: ~w~n", [X]),
            part2(Xs, Y),
            format("Task 2: ~w~n", [Y])
        )
    ),
    halt.

:- initialization(run).
